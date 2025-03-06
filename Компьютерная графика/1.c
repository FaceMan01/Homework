#include <GL/freeglut.h>
#include <stdlib.h>

// Функция для отрисовки
void display(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();

    // Рисуем квадрат
    glBegin(GL_QUADS);
        glColor3f(0.0f, 0.0f, 1.0f);
        glVertex2f(-0.9f, -0.9f);
        glVertex2f( 0.9f, -0.9f);
        glVertex2f( 0.9f,  0.9f);
        glVertex2f(-0.9f,  0.9f);
    glEnd();

    glutSwapBuffers();
}

// Функция обработки изменения размеров окна
void reshape(int w, int h) {
    if (h == 0) h = 1;  // Предотвращаем деление на ноль
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

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

// Функция обработки нажатия клавиш
void keyboard(unsigned char key, int x, int y) {
    // Если нажата клавиша Esc (ASCII 27) — выходим из приложения
    if (key == 27) {
        exit(0);
    }
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    // Используем двойную буферизацию и RGB режим
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Задание 1");

    // Регистрируем коллбэки
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);

    // Устанавливаем цвет фона (белый)
    glClearColor(1.0, 1.0, 1.0, 1.0);

    glutMainLoop();
    return 0;
}
