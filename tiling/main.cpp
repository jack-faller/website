// Must include this first.
#include <glad/gl.h>

#include <GLFW/glfw3.h>
#include <cstring>
#include <fstream>
#include <getopt.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <sys/types.h>
#include <vector>
using namespace glm;

#define SHADER_DIR "tiling/"

option options[] = {
	{ "output", required_argument, 0, 'o' },
	{ "copies", required_argument, 0, 'c' },
};

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define LENGTH(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))

int positive_mod(int numerator, int denominator) {
	int out = numerator % denominator;
	return out >= 0 ? out : out + denominator;
}

template <typename T> struct Image {
	int width, height;
	T *buffer;
	Image(int width, int height) : width(width), height(height) {
		buffer = new T[width * height];
	}
	~Image() { delete[] buffer; }
	T &at(int x, int y) { return buffer[width * y + x]; }
	T &wrapped_at(int x, int y) {
		return buffer[width * positive_mod(y, height) + positive_mod(x, width)];
	}
	template <typename F> void for_each(F f) {
		for (int y = 0; y < height; ++y)
			for (int x = 0; x < width; ++x)
				f(at(x, y));
	}
	template <typename F, typename U> void for_each(Image<U> &other, F f) {
		for (int y = 0; y < height; ++y)
			for (int x = 0; x < width; ++x)
				f(at(x, y), other.at(x, y));
	}
	// Flip image vertically by swapping rows of pixels.
	void flip() {
		for (int y = 0; y < height / 2; ++y)
			std::swap_ranges(&at(0, y), &at(width, y), &at(0, height - y - 1));
	}
};

struct Dither {
// Sierra dither kernel
#define DITHER1 0, 0, 0, 5, 3,
#define DITHER2 2, 4, 5, 4, 2,
#define DITHER3 0, 2, 3, 2, 0,
	constexpr static const int values[3][5]
		= { { DITHER1 }, { DITHER2 }, { DITHER3 } };
	static const int x_mid = 2, y_mid = 0;
	static const int width = LENGTH(values[0]), height = LENGTH(values);
	template <typename T> static T apply(T val, int x, int y) {
		return val * values[y][x] / 32;
	}
	static const int threshold = 128;
};

std::string read_file(const char *path) {
	std::string out;
	std::ifstream stream(path, std::ios::in);
	if (stream.is_open()) {
		std::stringstream sstr;
		sstr << stream.rdbuf();
		out = sstr.str();
		stream.close();
	} else {
		eprintf("Couldn't read file %s.\n", path);
		exit(0);
	}
	return out;
}

GLuint load_shader(const char *path, GLenum shader_type) {
	GLuint id = glCreateShader(shader_type);
	std::string code = read_file(path);

	// Compile Vertex Shader
	eprintf("Compiling shader: %s\n", path);
	const GLchar *s = code.c_str();
	glShaderSource(id, 1, &s, NULL);
	glCompileShader(id);

	// Check Vertex Shader
	GLint result = GL_FALSE;
	int log_length;
	glGetShaderiv(id, GL_COMPILE_STATUS, &result);
	glGetShaderiv(id, GL_INFO_LOG_LENGTH, &log_length);
	if (log_length > 0) {
		std::vector<char> error_message(log_length + 1);
		glGetShaderInfoLog(id, log_length, NULL, &error_message[0]);
		eprintf("%s\n", &error_message[0]);
		exit(1);
	}
	return id;
}

GLuint load_shaders(const char *phase) {
	// Create the shaders
	std::stringstream vert_path;
	std::stringstream frag_path;
	vert_path << SHADER_DIR << phase << ".vert";
	frag_path << SHADER_DIR << phase << ".frag";
	GLuint vertex_id = load_shader(vert_path.str().c_str(), GL_VERTEX_SHADER);
	GLuint fragment_id
		= load_shader(frag_path.str().c_str(), GL_FRAGMENT_SHADER);

	// Link the program
	eprintf("Linking program\n");
	GLuint program_id = glCreateProgram();
	glAttachShader(program_id, vertex_id);
	glAttachShader(program_id, fragment_id);
	glLinkProgram(program_id);

	// Check the program
	GLint result = GL_FALSE;
	int log_length;
	glGetProgramiv(program_id, GL_LINK_STATUS, &result);
	glGetProgramiv(program_id, GL_INFO_LOG_LENGTH, &log_length);
	if (log_length > 0) {
		std::vector<char> error_message(log_length + 1);
		glGetProgramInfoLog(program_id, log_length, NULL, &error_message[0]);
		eprintf("%s\n", &error_message[0]);
	}

	glDetachShader(program_id, vertex_id);
	glDetachShader(program_id, fragment_id);

	glDeleteShader(vertex_id);
	glDeleteShader(fragment_id);

	return program_id;
}

void GLAPIENTRY print_gl_error(
	GLenum source,
	GLenum type,
	GLuint id,
	GLenum severity,
	GLsizei length,
	const GLchar *message,
	const void *userParam
) {
	eprintf(
		"GL CALLBACK: %s type = 0x%x, severity = 0x%x, message = %s\n",
		(type == GL_DEBUG_TYPE_ERROR ? "ERROR" : ""),
		type,
		severity,
		message
	);
}

struct GLProgram {
	GLuint program;
	GLProgram(const char *file) { program = load_shaders(file); }
	~GLProgram() { glDeleteProgram(program); }
	GLuint uniform(const char *name) {
		return glGetUniformLocation(program, name);
	}
	void use() { glUseProgram(program); }
};

struct GLAttribArrayEnable {
	int array;
	int divisor = 0;
	GLAttribArrayEnable(int array) : array(array) {}
	~GLAttribArrayEnable() {
		glDisableVertexAttribArray(array);
		if (divisor != 0)
			glVertexAttribDivisor(array, 0);
	}
	void set_divisor(int n) { glVertexAttribDivisor(array, divisor = n); }
};
template <int n> struct GLBuffer {
	GLuint v;
	GLBuffer(std::vector<glm::vec<n, float>> &data) {
		glGenBuffers(1, &v);
		glBindBuffer(GL_ARRAY_BUFFER, v);
		glBufferData(
			GL_ARRAY_BUFFER,
			data.size() * sizeof(data[0]),
			&data[0],
			GL_STATIC_DRAW
		);
	}
	~GLBuffer() { glDeleteBuffers(1, &v); }
	GLAttribArrayEnable bind(int array) {
		glEnableVertexAttribArray(array);
		glBindBuffer(GL_ARRAY_BUFFER, v);
		glVertexAttribPointer(array, n, GL_FLOAT, GL_FALSE, 0, nullptr);
		return { array };
	}
};

void push_rotate(
	std::vector<vec3> &dst, std::vector<vec3> src, float angle, vec3 axis
) {
	mat4 matrix = rotate(mat4(1), angle, axis);
	for (auto i : src)
		dst.push_back(matrix * vec4(i, 0));
}

enum Index { NORMAL, IMAGE };
int main(int argc, char **argv) {
	int copies = 1;
	vec<2, int> screen(108, 0);
	screen.y = screen.x * tan(radians(30.0));
	char *output = nullptr;
	for (int val; -1 != (val = getopt_long(argc, argv, "", options, nullptr));)
		switch (val) {
		case 'o': output = optarg; break;
		case 'c':
			if (!sscanf(optarg, "%d", &copies))
				eprintf("Non-integer argument to copies.\n");
			break;
		}

	if (!glfwInit()) {
		eprintf("Failed to initialise glfw\n");
		return 1;
	}
	glfwWindowHint(GLFW_SAMPLES, 1);
	glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	GLFWwindow *window = glfwCreateWindow(
		screen.x * copies, screen.y * copies, "Tiling", NULL, NULL
	);
	if (window == nullptr) {
		eprintf("Failed to open GLFW window\n");
		glfwTerminate();
		return 1;
	}
	if (output != nullptr)
		glfwHideWindow(window);
	glfwMakeContextCurrent(window);

	int version = gladLoadGL(glfwGetProcAddress);
	eprintf(
		"Using GL %d.%d\n",
		GLAD_VERSION_MAJOR(version),
		GLAD_VERSION_MINOR(version)
	);

	glEnable(GL_DEBUG_OUTPUT);
	glDebugMessageCallback(print_gl_error, nullptr);

	glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

	glEnable(GL_DEPTH_TEST);

	GLuint vertex_array;
	glGenVertexArrays(1, &vertex_array);
	glBindVertexArray(vertex_array);

	vec2 view(0, 5);
	view.x = view.y * screen.x / screen.y;
	float sphere_dist = view.x / sin(radians(45.0));

	float beam_width = 2;
	float sphere_radius = 4;

	float beam_length = sphere_dist / 2;
	std::vector<vec3> beams;
	/* Construct the beams by rotating boxes. */ {
		std::vector<vec3> tri {
			vec3(beam_length, beam_width / 2, beam_width / 2),
			vec3(-beam_length, beam_width / 2, beam_width / 2),
			vec3(beam_length, -beam_width / 2, beam_width / 2),
		};
		std::vector<vec3> face;
		push_rotate(face, tri, 0, vec3(1, 0, 0));
		push_rotate(face, tri, radians(180.0), vec3(0, 0, 1));
		std::vector<vec3> box;
		push_rotate(box, face, 0, vec3(1, 0, 0));
		push_rotate(box, face, radians(-90.0), vec3(1, 0, 0));

		push_rotate(beams, box, 0, vec3(1, 0, 0));
		push_rotate(beams, box, radians(90.0), vec3(0, 1, 0));
		push_rotate(beams, box, radians(-90.0), vec3(0, 0, 1));
	}
	std::vector<vec3> normals;
	for (int i = 0; i < beams.size() / 3; ++i)
		for (int j = 0; j < 3; ++j)
			normals.push_back(normalize(cross(
				beams[i * 3 + 1] - beams[i * 3], beams[i * 3 + 2] - beams[i * 3]
			)));
	std::vector<vec3> fullscreen_tris {
		vec3(1, 1, 0),  vec3(-1, 1, 0), vec3(1, -1, 0),
		vec3(-1, 1, 0), vec3(1, -1, 0), vec3(-1, -1, 0),
	};
	std::vector<vec2> square {
		vec2(1, 1), vec2(-1, 1), vec2(-1, -1),
		vec2(1, 1), vec2(1, -1), vec2(-1, -1),
	};
	// Last component is radius, rest are position.
	std::vector<vec4> spheres { vec4(0, 0, 0, sphere_radius) };
	for (int sign = -1; sign <= 1; sign += 2)
		for (int ax = 0; ax < 3; ++ax)
			spheres.push_back(vec4(0, 0, 0, sphere_radius)),
				spheres.back()[ax] = sign * sphere_dist;
	GLBuffer input_vertices(beams);
	GLBuffer input_normals(normals);
	GLBuffer input_fullscreen(fullscreen_tris);
	GLBuffer input_square(square);
	GLBuffer input_spheres(spheres);

	GLProgram triangles_program("triangles");
	GLuint triangles_transform = triangles_program.uniform("transform");
	GLuint triangles_perspective = triangles_program.uniform("perspective");
	GLProgram spheres_program("spheres");
	GLuint spheres_transform = spheres_program.uniform("transform");
	GLuint spheres_perspective = spheres_program.uniform("perspective");
	GLProgram colour_program("colour");
	GLuint colour_normals = colour_program.uniform("normals");
	GLuint colour_light_direction = colour_program.uniform("light_direction");

	GLuint colour_textures[2];
	glGenTextures(2, colour_textures);
	for (int i = 0; i < 2; ++i) {
		glBindTexture(GL_TEXTURE_2D, colour_textures[i]);
		glTexImage2D(
			GL_TEXTURE_2D,
			0,
			GL_RGBA,
			screen.x,
			screen.y,
			0,
			GL_RGBA,
			GL_UNSIGNED_BYTE,
			nullptr
		);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	}
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	// Didn't end up needing this, could be a regular depth buffer.
	GLuint depth_texture;
	glGenTextures(1, &depth_texture);
	glBindTexture(GL_TEXTURE_2D, depth_texture);
	glTexImage2D(
		GL_TEXTURE_2D,
		0,
		GL_DEPTH_COMPONENT24,
		screen.x,
		screen.y,
		0,
		GL_DEPTH_COMPONENT,
		GL_FLOAT,
		0
	);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

	GLuint frame_buffers[2];
	glGenFramebuffers(LENGTH(frame_buffers), frame_buffers);
	for (int i = 0; i < 2; ++i) {
		glBindFramebuffer(GL_FRAMEBUFFER, frame_buffers[i]);
		glFramebufferTexture(
			GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, colour_textures[i], 0
		);
		const GLenum draw_buffers[] = { GL_COLOR_ATTACHMENT0 };
		glDrawBuffers(LENGTH(draw_buffers), draw_buffers);
	}

	glBindFramebuffer(GL_FRAMEBUFFER, frame_buffers[NORMAL]);
	glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depth_texture, 0);
	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
		eprintf("Frame-buffer setup failed.\n");
		return 1;
	}

	mat4 perspective_matrix
		= ortho(-view.x, view.x, -view.y, view.y, 0.1f, 100.0f);
	mat4 transform_matrix
		= lookAt(vec3(10, 10, 10), vec3(0, 0, 0), vec3(0, 1, 0));

	do {
		glBindFramebuffer(GL_FRAMEBUFFER, frame_buffers[NORMAL]);
		glViewport(0, 0, screen.x, screen.y);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		/* Draw the triangle normals. */ {
			triangles_program.use();
			glUniformMatrix4fv(
				triangles_perspective, 1, GL_FALSE, &perspective_matrix[0][0]
			);
			glUniformMatrix4fv(
				triangles_transform, 1, GL_FALSE, &transform_matrix[0][0]
			);

			GLAttribArrayEnable enable_zero = input_vertices.bind(0);
			GLAttribArrayEnable enable_one = input_normals.bind(1);
			GLAttribArrayEnable enable_two = input_spheres.bind(2);
			enable_two.set_divisor(1);

			glDrawArraysInstanced(
				GL_TRIANGLES, 0, beams.size(), spheres.size()
			);
		}

		/* Draw the spheres. */ {
			spheres_program.use();
			glUniformMatrix4fv(
				spheres_perspective, 1, GL_FALSE, &perspective_matrix[0][0]
			);
			glUniformMatrix4fv(
				spheres_transform, 1, GL_FALSE, &transform_matrix[0][0]
			);

			GLAttribArrayEnable enable_zero = input_square.bind(0);
			GLAttribArrayEnable enable_one = input_spheres.bind(1);
			enable_one.set_divisor(1);

			glDrawArraysInstanced(
				GL_TRIANGLES, 0, square.size(), spheres.size()
			);
		}

		glBindFramebuffer(GL_FRAMEBUFFER, frame_buffers[IMAGE]);
		glViewport(0, 0, screen.x, screen.y);
		glClear(GL_COLOR_BUFFER_BIT);

		/* Render colours based on normals. */ {
			colour_program.use();

			glActiveTexture(GL_TEXTURE0);
			glBindTexture(GL_TEXTURE_2D, colour_textures[NORMAL]);
			glUniform1i(colour_normals, 0);

			vec4 light = normalize(transform_matrix * vec4(0.7, 1, 1, 0));
			glUniform4fv(colour_light_direction, 1, &light[0]);

			glActiveTexture(GL_TEXTURE1);

			GLAttribArrayEnable _zero = input_fullscreen.bind(0);

			glDrawArrays(GL_TRIANGLES, 0, fullscreen_tris.size());
		}

		if (output != nullptr) {
			FILE *f = fopen(output, "w");
			output = nullptr;

			// PPM header.
			fprintf(f, "P6\n%d %d\n255\n", screen.x, screen.y);

			Image<uint8_t[3]> image(screen.x, screen.y);
			Image<uint8_t> grey(screen.x, screen.y);
			glReadBuffer(GL_COLOR_ATTACHMENT0);
			glReadPixels(
				0, 0, screen.x, screen.y, GL_RGB, GL_UNSIGNED_BYTE, image.buffer
			);
			image.for_each(grey, [](uint8_t(&a)[3], uint8_t &b) { b = a[0]; });
			grey.flip();
			// Dithering.
			// Choose 0.467 because it makes the edges of the image tile without
			// causing a clear border.
			grey.for_each([](uint8_t &v) { v = v * 0.467; });
			for (int y = 0; y < grey.height; ++y) {
				for (int x = 0; x < grey.width; ++x) {
					int val = grey.at(x, y);
					int new_val = val < Dither::threshold ? 0 : 255;
					grey.at(x, y) = new_val;
					int error = val - new_val;
					for (int dither_y = 0; dither_y < Dither::height;
					     ++dither_y) {
						int target_y = y - Dither::y_mid + dither_y;
						if (target_y < 0 || target_y >= screen.y)
							continue;
						for (int dither_x = 0; dither_x < Dither::width;
						     ++dither_x) {
							int target_x = x - Dither::x_mid + dither_x;
							if (target_x < 0 || target_x >= screen.x)
								continue;
							int target_val = grey.at(target_x, target_y);
							grey.at(target_x, target_y) = clamp(
								target_val
									+ Dither::apply(error, dither_x, dither_y),
								0,
								255
							);
						}
					}
				}
			}
			// Dim the entire image because pure white is too distracting.
			grey.for_each([](uint8_t &v) { v = v * 0.5; });
			image.for_each(grey, [](uint8_t(&a)[3], uint8_t &b) {
				memset(&a, b, 3);
			});
			fwrite(image.buffer, sizeof(*image.buffer), screen.x * screen.y, f);
			fclose(f);
			break;
		}

		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glViewport(0, 0, screen.x * copies, screen.y * copies);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
		for (int x = 0; x < copies; ++x)
			for (int y = 0; y < copies; ++y)
				glBlitNamedFramebuffer(
					frame_buffers[IMAGE],
					0,
					0,
					0,
					screen.x,
					screen.y,
					screen.x * x,
					screen.y * y,
					screen.x * (x + 1),
					screen.y * (y + 1),
					GL_COLOR_BUFFER_BIT,
					GL_NEAREST
				);

		glfwSwapBuffers(window);
		glfwWaitEvents();
	} while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS
	         && glfwGetKey(window, GLFW_KEY_Q) != GLFW_PRESS
	         && glfwWindowShouldClose(window) == 0);

	glDeleteFramebuffers(LENGTH(frame_buffers), frame_buffers);
	glDeleteTextures(LENGTH(colour_textures), colour_textures);
	glDeleteTextures(1, &depth_texture);

	glfwTerminate();
}
