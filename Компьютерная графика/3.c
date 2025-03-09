#include <GL/glut.h>
#include <math.h>
#include <stdio.h>

float eyex = 5.0f, eyey = 5.0f, eyez = 5.0f;
int projectionMode = 0; // 0 - Perspective, 1 - Orthographic

void drawCube() {
    glBegin(GL_QUADS);
    // Front face
    glColor3f(1, 0, 0);
    glVertex3f(-1, -1, 1); glVertex3f(1, -1, 1);
    glVertex3f(1, 1, 1); glVertex3f(-1, 1, 1);
    
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
    glutCreateWindow("Задание 3");
    glEnable(GL_DEPTH_TEST);
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);
    glutMainLoop();
    return 0;
}
