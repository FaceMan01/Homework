#define STB_IMAGE_IMPLEMENTATION
#define _USE_MATH_DEFINES
#include <cmath>
#include <iostream>
#include <GL/glut.h>
#include "stb_image.h"
#include <vector>
#include <string>

void reshape(int w, int h);

float eyex = 4.0f, eyey = 4.0f, eyez = 4.0f;
float centerx = 0.0f, centery = 0.0f, centerz = 0.0f;
float upx = 0.0f, upy = 1.0f, upz = 0.0f;

float horizontalAngle = 0.0f;
float verticalAngle = 0.0f;

float radius = 4.0f;

bool perspectiveProjection = true;

GLuint textures[3];

GLuint loadTexture(const char* filename) {
    int width, height, channels;
    unsigned char* data = stbi_load(filename, &width, &height, &channels, 0);
    if (!data) {
        std::cerr << "Failed to load texture: " << filename << std::endl;
        return 0;
    }

    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    if (channels == 3) {
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, data);
    }
    else if (channels == 4) {
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
    }

    stbi_image_free(data);

    return texture;
}

void drawCube() {

    glBindTexture(GL_TEXTURE_2D, textures[0]);
    glBegin(GL_QUADS);
    glTexCoord2f(1.0f, 1.0f); glVertex3f(1.0f, -1.0f, 1.0f);
    glTexCoord2f(0.0f, 1.0f); glVertex3f(1.0f, -1.0f, -1.0f);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(1.0f, 1.0f, -1.0f);
    glTexCoord2f(1.0f, 0.0f); glVertex3f(1.0f, 1.0f, 1.0f);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
    glTexCoord2f(5.0f, 5.0f); glVertex3f(-1.0f, -1.0f, 1.0f);
    glTexCoord2f(0.0f, 5.0f); glVertex3f(1.0f, -1.0f, 1.0f);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(1.0f, 1.0f, 1.0f);
    glTexCoord2f(5.0f, 0.0f); glVertex3f(-1.0f, 1.0f, 1.0f);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, textures[0]);
    glBegin(GL_QUADS);
    glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f, 1.0f);
    glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
    glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f, 1.0f, -1.0f);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, 1.0f, 1.0f);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, textures[2]);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
    glTexCoord2f(2.0f, 0.0f); glVertex3f(1.0f, -1.0f, -1.0f);
    glTexCoord2f(2.0f, 2.0f); glVertex3f(1.0f, 1.0f, -1.0f);
    glTexCoord2f(0.0f, 2.0f); glVertex3f(-1.0f, 1.0f, -1.0f);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, 1.0f, 1.0f);
    glTexCoord2f(1.0f, 0.0f); glVertex3f(1.0f, 1.0f, 1.0f);
    glTexCoord2f(1.0f, 1.0f); glVertex3f(1.0f, 1.0f, -1.0f);
    glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f, 1.0f, -1.0f);
    glEnd();

    glBindTexture(GL_TEXTURE_2D, textures[1]);
    glBegin(GL_QUADS);
    glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f, 1.0f);
    glTexCoord2f(1.0f, 1.0f); glVertex3f(1.0f, -1.0f, 1.0f);
    glTexCoord2f(0.0f, 1.0f); glVertex3f(1.0f, -1.0f, -1.0f);
    glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
    glEnd();
}

void updateCameraPosition() {
    eyex = radius * cos(verticalAngle) * sin(horizontalAngle);
    eyey = radius * sin(verticalAngle);
    eyez = radius * cos(verticalAngle) * cos(horizontalAngle);
    if (cos(verticalAngle) < 0) {
        upy = -1.0f;
    }
    else {
        upy = 1.0f;
    }
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz);

    drawCube();

    glutSwapBuffers();
}

void keyboard(unsigned char key, int x, int y) {
    float angleStep = 0.1f;
    float radiusStep = 0.1f;
    
    switch (key) {
        case 'a':
            horizontalAngle -= angleStep;
            break;
        case 'd':
            horizontalAngle += angleStep;
            break;
        case 'w':
            verticalAngle += angleStep;
            break;
        case 's':
            verticalAngle -= angleStep;
            break;
        case 'q':
            radius -= radiusStep;
            break;
        case 'e':
            radius += radiusStep;
            break;
        case 'r':
            perspectiveProjection = !perspectiveProjection;
            reshape(glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT));
            glutPostRedisplay();
            break;
        case 27:
            exit(0);
            break;
        }
    
        if (verticalAngle > 2 * M_PI) verticalAngle -= 2 * M_PI;
        if (verticalAngle < -2 * M_PI) verticalAngle += 2 * M_PI;
    
        updateCameraPosition();
        glutPostRedisplay();
    }
    
    void reshape(int w, int h) {
        glViewport(0, 0, w, h);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
    
        float aspectRatio = (float)w / (float)h;
        if (perspectiveProjection) {
            gluPerspective(45.0f, (float)w / (float)h, 0.1f, 100.0f);
        }
        else {
            glOrtho(-2.5, 2.5, -2.5, 2.5, -5.0, 5.0);
        }
    
        glMatrixMode(GL_MODELVIEW);
    }
    
    int main(int argc, char** argv) {
        glutInit(&argc, argv);
        glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
        glutInitWindowSize(800, 600);
        glutCreateWindow("Задание 4");
    
        glEnable(GL_DEPTH_TEST);
        glEnable(GL_TEXTURE_2D);
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    
        std::vector<std::string> texturePaths = {
            "./textures/1.jpg",
            "./textures/2.jpg",
            "./textures/3.jpg"
        };
    
        for (int i = 0; i < texturePaths.size(); i++) {
            textures[i] = loadTexture(texturePaths[i].c_str());
        }
    
        glBindTexture(GL_TEXTURE_2D, textures[2]);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    
        updateCameraPosition();
    
        glutDisplayFunc(display);
        glutReshapeFunc(reshape);
        glutKeyboardFunc(keyboard);
    
        glutMainLoop();
        return 0;
    }