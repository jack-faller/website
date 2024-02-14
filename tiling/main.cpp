// Must include this first.
#include <glad/gl.h>

#include <GLFW/glfw3.h>
#include <fstream>
#include <getopt.h>
#include <glm/glm.hpp>
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <sys/types.h>
#include <vector>
using namespace glm;

#define VERTEX_PATH "tiling/shader.vert"
#define FRAGMENT_PATH "tiling/shader.frag"

option options[] = {
    {"size", required_argument, 0, 's'},
    {"output", required_argument, 0, 'o'},
};

#define eprintf(...) fprintf(stderr, __VA_ARGS__)

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

GLuint load_shaders() {
  // Create the shaders
  GLuint vertex_id = load_shader(VERTEX_PATH, GL_VERTEX_SHADER);
  GLuint fragment_id = load_shader(FRAGMENT_PATH, GL_FRAGMENT_SHADER);

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

void render(GLuint vertex_buffer, GLuint program_id) {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glUseProgram(program_id);

  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void *)0);

  glDrawArrays(GL_TRIANGLES, 0, 3);
  glDisableVertexAttribArray(0);
}

int main(int argc, char **argv) {
  int width = 100;
  int height = 100;
  char *output = nullptr;
  int val;
  while (-1 != (val = getopt_long(argc, argv, "", options, nullptr)))
    switch (val) {
    case 's':
      if (!sscanf(optarg, "%dx%d", &width, &height))
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
  glfwWindowHint(GLFW_SAMPLES, 4);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
  glfwWindowHintString(GLFW_X11_CLASS_NAME, "floating");
  glfwWindowHintString(GLFW_X11_INSTANCE_NAME, "floating");
  GLFWwindow *window = glfwCreateWindow(width, height, "Tiling", NULL, NULL);
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

  static const GLfloat vertices[] = {
      -1.0f, -1.0f, 0.0f, 1.0f, -1.0f, 0.0f, 0.0f, 1.0f, 0.0f,
  };
  GLuint vertex_buffer;
  glGenBuffers(1, &vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

  GLuint program_id = load_shaders();

  do {
    render(vertex_buffer, program_id);

    if (output != nullptr) {
      uint8_t *buf = new uint8_t[width * height * 4];
      glReadBuffer(GL_BACK);
      glReadPixels(0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, buf);
      FILE *f = fopen(output, "w");
      fwrite(buf, sizeof(uint8_t), width * height * 4, f);
      fclose(f);
      delete[] buf;
      break;
    }

    glfwSwapBuffers(window);
    glfwPollEvents();
  } while (glfwGetKey(window, GLFW_KEY_ESCAPE) != GLFW_PRESS &&
           glfwWindowShouldClose(window) == 0);
}
