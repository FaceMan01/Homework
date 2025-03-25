#include <GL/freeglut.h>
#include <stdlib.h>

void display(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();

    glBegin(GL_QUADS);
        glColor3f(0.0f, 0.0f, 1.0f);
        glVertex2f(-0.9f, -0.9f);
        glVertex2f( 0.9f, -0.9f);
        glVertex2f( 0.9f,  0.9f);
        glVertex2f(-0.9f,  0.9f);
    glEnd();

    glutSwapBuffers();
}

void reshape(int w, int h) {
    if (h == 0) h = 1;
    glViewport(0, 0, w, h);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    // Поддерживаем квадратную проекцию независимо от соотношения сторон окна
    if (w <= h) {
        float ratio = (float)h / (float)w;
        glOrtho(-1.0, 1.0, -ratio, ratio, -1.0, 1.0);
    } else {
        float ratio = (float)w / (float)h;
        glOrtho(-ratio, ratio, -1.0, 1.0, -1.0, 1.0);
    }

}

void keyboard(unsigned char key, int x, int y) {
    if (key == 27) {
        exit(0);
    }
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Задание 1");

    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);

    glClearColor(1.0, 1.0, 1.0, 1.0);

    glutMainLoop();
    return 0;
}
