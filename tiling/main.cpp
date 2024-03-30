// Must include this first.
#include <glad/gl.h>

#include <GLFW/glfw3.h>
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
	{ "size", required_argument, 0, 's' },
	{ "output", required_argument, 0, 'o' },
	{ "copies", required_argument, 0, 'c' },
};

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#define LENGTH(ARRAY) (sizeof(ARRAY) / sizeof(ARRAY[0]))

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
		return 0;
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

int main(int argc, char **argv) {
	int copies = 1;
	vec<2, int> screen = { 74, 108 };
	char *output = nullptr;
	for (int val; -1 != (val = getopt_long(argc, argv, "", options, nullptr));)
		switch (val) {
		case 's':
			if (!sscanf(optarg, "%dx%d", &screen.x, &screen.y))
				eprintf("Incorrect size specification\n");
			break;
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

	static const vec3 vertices[] = {
		-vec3(1.0f, -1.0f, 0.0f), -vec3(-1.0f, -1.0f, 0.0f),
		-vec3(0.0f, 1.0f, 0.0f),  vec3(1.0f, -1.0f, 0.0f),
		vec3(-1.0f, -1.0f, 0.0f), vec3(0.0f, 1.0f, 0.0f),
		vec3(0.0f, -1.0f, 1.0f),  vec3(0.0f, -1.0f, -1.0f),
		vec3(0.0f, 1.0f, 0.0f),
	};
	static vec3 normals[sizeof(vertices)];
	for (int i = 0; i < LENGTH(normals); ++i) {
#define VERT(num) vertices[i / 3 * 3 + num]
		normals[i] = normalize(cross(VERT(1) - VERT(0), VERT(2) - VERT(0)));
#undef VERT
	}
	static vec3 fullscreen_tris[] = {
		vec3(1, 1, 0),  vec3(-1, 1, 0), vec3(1, -1, 0),
		vec3(-1, 1, 0), vec3(1, -1, 0), vec3(-1, -1, 0),
	};
	GLuint input_vertices;
	glGenBuffers(1, &input_vertices);
	glBindBuffer(GL_ARRAY_BUFFER, input_vertices);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
	GLuint input_normals;
	glGenBuffers(1, &input_normals);
	glBindBuffer(GL_ARRAY_BUFFER, input_normals);
	glBufferData(GL_ARRAY_BUFFER, sizeof(normals), normals, GL_STATIC_DRAW);
	GLuint input_fullscreen;
	glGenBuffers(1, &input_fullscreen);
	glBindBuffer(GL_ARRAY_BUFFER, input_fullscreen);
	glBufferData(
		GL_ARRAY_BUFFER,
		sizeof(fullscreen_tris),
		fullscreen_tris,
		GL_STATIC_DRAW
	);

	GLuint triangles_program = load_shaders("triangles");
	GLuint triangles_transform
		= glGetUniformLocation(triangles_program, "transform");
	GLuint triangles_perspective
		= glGetUniformLocation(triangles_program, "perspective");
	GLuint colour_program = load_shaders("colour");
	GLuint colour_normals = glGetUniformLocation(colour_program, "normals");
	GLuint colour_light_direction
		= glGetUniformLocation(colour_program, "light_direction");
	GLuint colour_dither_map
		= glGetUniformLocation(colour_program, "dither_map");

	float view_height = 5;

	GLuint frame_buffer;
	glGenFramebuffers(1, &frame_buffer);
	glBindFramebuffer(GL_FRAMEBUFFER, frame_buffer);
	GLuint normal_texture;
	glGenTextures(1, &normal_texture);
	glBindTexture(GL_TEXTURE_2D, normal_texture);
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
	glFramebufferTexture(
		GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, normal_texture, 0
	);

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

	glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depth_texture, 0);

	const GLenum draw_buffers[] = { GL_COLOR_ATTACHMENT0 };
	glDrawBuffers(LENGTH(draw_buffers), draw_buffers);

	if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
		eprintf("Frame-buffer setup failed.\n");
		return 1;
	}

	float ratio = (float)screen.x / screen.y;
	mat4 perspective_matrix = ortho(
		-view_height * ratio,
		view_height * ratio,
		-view_height,
		view_height,
		0.1f,
		100.0f
	);
	mat4 transform_matrix = lookAt(vec3(4, 4, 4), vec3(0, 0, 0), vec3(0, 1, 0));

	float dither_map[25] = { 0 };
	for (int i = 0; i < LENGTH(dither_map); ++i) {
		int j = 0, count = 0, pick = random() % (LENGTH(dither_map) - i);
		for (; count < pick; ++j)
			if (dither_map[j] == 0)
				++count;
		dither_map[j] = (i + 1.) / 25;
	}

	do {
		glBindFramebuffer(GL_FRAMEBUFFER, frame_buffer);
		glViewport(0, 0, screen.x, screen.y);

		/* Draw the triangle normals. */ {
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

			glUseProgram(triangles_program);
			glUniformMatrix4fv(
				triangles_perspective, 1, GL_FALSE, &perspective_matrix[0][0]
			);
			glUniformMatrix4fv(
				triangles_transform, 1, GL_FALSE, &transform_matrix[0][0]
			);

			glEnableVertexAttribArray(0);
			glBindBuffer(GL_ARRAY_BUFFER, input_vertices);
			glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

			glEnableVertexAttribArray(1);
			glBindBuffer(GL_ARRAY_BUFFER, input_normals);
			glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

			glDrawArrays(GL_TRIANGLES, 0, LENGTH(vertices));

			glDisableVertexAttribArray(0);
			glDisableVertexAttribArray(1);
		}

		glBindFramebuffer(GL_FRAMEBUFFER, 0);
		glViewport(0, 0, screen.x * copies, screen.y * copies);

		/* Render colours based on normals. */ {
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

			glUseProgram(colour_program);

			glActiveTexture(GL_TEXTURE0);
			glBindTexture(GL_TEXTURE_2D, normal_texture);
			glUniform1i(colour_normals, 0);
			vec4 light = normalize(transform_matrix * vec4(1, 1, 1, 0));
			glUniform4fv(colour_light_direction, 1, &light[0]);
			glUniform1fv(colour_dither_map, LENGTH(dither_map), dither_map);

			glEnableVertexAttribArray(0);
			glBindBuffer(GL_ARRAY_BUFFER, input_fullscreen);
			glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

			glDrawArrays(GL_TRIANGLES, 0, LENGTH(fullscreen_tris));
			glDisableVertexAttribArray(0);
		}

		if (output != nullptr) {
			uint8_t *buf = new uint8_t[screen.x * screen.y * 4];
			glReadBuffer(GL_BACK);
			glReadPixels(
				0, 0, screen.x, screen.y, GL_RGBA, GL_UNSIGNED_BYTE, buf
			);
			FILE *f = fopen(output, "w");
			fwrite(buf, sizeof(uint8_t), screen.x * screen.y * 4, f);
			fclose(f);
			delete[] buf;
			break;
		}

		glfwSwapBuffers(window);
		glfwWaitEvents();
	} while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS
	         && glfwGetKey(window, GLFW_KEY_Q) != GLFW_PRESS
	         && glfwWindowShouldClose(window) == 0);

	glDeleteBuffers(1, &input_vertices);
	glDeleteBuffers(1, &input_normals);
	glDeleteFramebuffers(1, &frame_buffer);
	glDeleteTextures(1, &normal_texture);
	glDeleteTextures(1, &depth_texture);
	glDeleteVertexArrays(1, &input_vertices);
	glDeleteProgram(triangles_program);

	glfwTerminate();
}
