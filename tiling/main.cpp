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
    {"size", required_argument, 0, 's'},
    {"output", required_argument, 0, 'o'},
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
  GLuint fragment_id = load_shader(frag_path.str().c_str(), GL_FRAGMENT_SHADER);

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

int main(int argc, char **argv) {
  vec<2, int> screen;
  char *output = nullptr;
  int val;
  while (-1 != (val = getopt_long(argc, argv, "", options, nullptr)))
    switch (val) {
    case 's':
      if (!sscanf(optarg, "%dx%d", &screen.x, &screen.y))
        eprintf("Incorrect size specification\n");
      break;
    case 'o':
      output = optarg;
      break;
    }

  if (!glfwInit()) {
    eprintf("Failed to initialise glfw\n");
    return 1;
  }
  glfwWindowHint(GLFW_SAMPLES, 1);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHintString(GLFW_X11_CLASS_NAME, "floating");
  glfwWindowHintString(GLFW_X11_INSTANCE_NAME, "floating");
  GLFWwindow *window =
      glfwCreateWindow(screen.x, screen.y, "Tiling", NULL, NULL);
  if (window == nullptr) {
    eprintf("Failed to open GLFW window\n");
    glfwTerminate();
    return 1;
  }
  if (output != nullptr)
    glfwHideWindow(window);
  glfwMakeContextCurrent(window);

  int version = gladLoadGL(glfwGetProcAddress);
  eprintf("Using GL %d.%d\n", GLAD_VERSION_MAJOR(version),
          GLAD_VERSION_MINOR(version));

  glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);
  glClearColor(0.0f, 0.0f, 0.4f, 1.0f);

  GLuint vertex_array;
  glGenVertexArrays(1, &vertex_array);
  glBindVertexArray(vertex_array);

  static const vec3 vertices[] = {
      -vec3(1.0f, -1.0f, 0.0f), -vec3(-1.0f, -1.0f, 0.0f),
      -vec3(0.0f, 1.0f, 0.0f),  vec3(1.0f, -1.0f, 0.0f),
      vec3(-1.0f, -1.0f, 0.0f), vec3(0.0f, 1.0f, 0.0f),
  };
  static vec3 normals[sizeof(vertices)];
  for (int i = 0; i < LENGTH(normals); ++i) {
#define VERT(num) vertices[i / 3 * 3 + num]
    normals[i] = normalize(cross(VERT(1) - VERT(0), VERT(2) - VERT(0)));
#undef VERT
  }
  GLuint vertex_buffer;
  glGenBuffers(1, &vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  GLuint normal_buffer;
  glGenBuffers(1, &normal_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, normal_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(normals), normals, GL_STATIC_DRAW);

  GLuint program = load_shaders("triangles");
  GLuint transform = glGetUniformLocation(program, "transform");
  GLuint perspective = glGetUniformLocation(program, "perspective");

  float view_height = 5;

  // GLuint normal_map;
  // glGenFramebuffers(1, &normal_map);
  // glBindFramebuffer(GL_FRAMEBUFFER, normal_map);
  glfwSetWindowUserPointer(window, &screen);
  glfwSetFramebufferSizeCallback(
      window, [](GLFWwindow *window, int width, int height) {
        glViewport(0, 0, width, height);
        vec<2, int> *screen = (vec<2, int> *)glfwGetWindowUserPointer(window);
        screen->x = width;
        screen->y = height;
      });

  do {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    float ratio = (float)screen.x / screen.y;
    mat4 perspective_matrix = ortho(-view_height * ratio, view_height * ratio,
                                    -view_height, view_height, 0.1f, 100.0f);
    mat4 transform_matrix =
         lookAt(vec3(4, 4, 4), vec3(0, 0, 0), vec3(0, 1, 0));

    glUseProgram(program);
    glUniformMatrix4fv(perspective, 1, GL_FALSE, &perspective_matrix[0][0]);
    glUniformMatrix4fv(transform, 1, GL_FALSE, &transform_matrix[0][0]);

    glEnableVertexAttribArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glEnableVertexAttribArray(1);
    glBindBuffer(GL_ARRAY_BUFFER, normal_buffer);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glDrawArrays(GL_TRIANGLES, 0, sizeof(vertices));
    glDisableVertexAttribArray(0);

    if (output != nullptr) {
      uint8_t *buf = new uint8_t[screen.x * screen.y * 4];
      glReadBuffer(GL_BACK);
      glReadPixels(0, 0, screen.x, screen.y, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      FILE *f = fopen(output, "w");
      fwrite(buf, sizeof(uint8_t), screen.x * screen.y * 4, f);
      fclose(f);
      delete[] buf;
      break;
    }

    glfwSwapBuffers(window);
    glfwPollEvents();
  } while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS &&
           glfwGetKey(window, GLFW_KEY_Q) != GLFW_PRESS &&
           glfwWindowShouldClose(window) == 0);

  glDeleteBuffers(1, &vertex_buffer);
  glDeleteBuffers(1, &normal_buffer);
  // glDeleteFramebuffers(1, &normal_map);
  glDeleteVertexArrays(1, &vertex_buffer);
  glDeleteProgram(program);

  glfwTerminate();
}
