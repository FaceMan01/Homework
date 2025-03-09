#include <GL/glut.h>
#include <math.h>
#include <stdio.h>
#include <SOIL/SOIL.h>

float eyex = 5.0f, eyey = 5.0f, eyez = 5.0f;
int projectionMode = 0; // 0 - Perspective, 1 - Orthographic
GLuint texture;

void loadTexture() {
    texture = SOIL_load_OGL_texture("brick.jpg", SOIL_LOAD_AUTO, SOIL_CREATE_NEW_ID, SOIL_FLAG_MIPMAPS);
    if (!texture) {
        printf("Texture loading failed!\n");
    }
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
}

void drawCube() {
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture);
    
    glBegin(GL_QUADS);
    // Front face with tiled texture
    glColor3f(1, 1, 1);
    glTexCoord2f(0, 0); glVertex3f(-1, -1, 1);
    glTexCoord2f(5, 0); glVertex3f(1, -1, 1);
    glTexCoord2f(5, 5); glVertex3f(1, 1, 1);
    glTexCoord2f(0, 5); glVertex3f(-1, 1, 1);
    
    glDisable(GL_TEXTURE_2D);
    
    // Back face
    glColor3f(0, 1, 0);
    glVertex3f(-1, -1, -1); glVertex3f(-1, 1, -1);
    glVertex3f(1, 1, -1); glVertex3f(1, -1, -1);
    
    // Left face
    glColor3f(0, 0, 1);
    glVertex3f(-1, -1, -1); glVertex3f(-1, -1, 1);
    glVertex3f(-1, 1, 1); glVertex3f(-1, 1, -1);
    
    // Right face
    glColor3f(1, 1, 0);
    glVertex3f(1, -1, -1); glVertex3f(1, 1, -1);
    glVertex3f(1, 1, 1); glVertex3f(1, -1, 1);
    
    // Top face
    glColor3f(1, 0, 1);
    glVertex3f(-1, 1, -1); glVertex3f(-1, 1, 1);
    glVertex3f(1, 1, 1); glVertex3f(1, 1, -1);
    
    // Bottom face
    glColor3f(0, 1, 1);
    glVertex3f(-1, -1, -1); glVertex3f(1, -1, -1);
    glVertex3f(1, -1, 1); glVertex3f(-1, -1, 1);
    
    glEnd();
}

void display() {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    gluLookAt(eyex, eyey, eyez, 0, 0, 0, 0, 1, 0);
    drawCube();
    glutSwapBuffers();
}

void reshape(int w, int h) {
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    if (projectionMode == 0)
        gluPerspective(45, (float)w/h, 1, 100);
    else
        glOrtho(-3, 3, -3, 3, 1, 100);
    glMatrixMode(GL_MODELVIEW);
}

void keyboard(unsigned char key, int x, int y) {
    float step = 0.5f;
    switch (key) {
        case 'w': eyey += step; break;
        case 's': eyey -= step; break;
        case 'a': eyex -= step; break;
        case 'd': eyex += step; break;
        case 'q': eyez += step; break;
        case 'e': eyez -= step; break;
        case 'p': projectionMode = 1 - projectionMode; reshape(800, 600); break;
        case 27: exit(0);
    }
    glutPostRedisplay();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(800, 600);
    glutCreateWindow("Cube Viewer");
    glEnable(GL_DEPTH_TEST);
    loadTexture();
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);
    glutMainLoop();
    return 0;
}
