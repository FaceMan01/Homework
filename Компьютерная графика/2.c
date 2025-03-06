#include <GL/freeglut.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

// Глобальные переменные
float square_angle = 0.0f;      // угол поворота квадрата
bool rotating = false;          // флаг вращения квадрата

// Позиция круга
float circle_x = 0.5f;
float circle_y = 0.0f;
float moveSpeed = 0.01f;        // скорость перемещения круга

// Флаги для перемещения круга
bool moveLeft  = false;
bool moveRight = false;
bool moveUp    = false;
bool moveDown  = false;

// Функция отрисовки квадрата
void drawSquare() {
    glBegin(GL_QUADS);
        glColor3f(1.0f, 0.0f, 0.0f);  // красный цвет
        glVertex2f(-0.25f, -0.25f);
        glVertex2f( 0.25f, -0.25f);
        glVertex2f( 0.25f,  0.25f);
        glVertex2f(-0.25f,  0.25f);
    glEnd();
}

// Функция отрисовки круга (с использованием многоугольника)
void drawCircle() {
    int num_segments = 100;
    float radius = 0.25f;
    glBegin(GL_POLYGON);
        glColor3f(0.0f, 0.0f, 1.0f);  // синий цвет
        for (int i = 0; i < num_segments; i++) {
            float theta = 2.0f * 3.1415926f * (float)i / (float)num_segments;
            float x = radius * cosf(theta);
            float y = radius * sinf(theta);
            glVertex2f(x, y);
        }
    glEnd();
}

// Основная функция отображения
void display(void) {
    glClear(GL_COLOR_BUFFER_BIT);
    glLoadIdentity();

    // Отрисовка квадрата с поворотом
    glPushMatrix();
        // Перенос квадрата в левую часть экрана
        glTranslatef(-0.5f, 0.0f, 0.0f);
        glRotatef(square_angle, 0.0f, 0.0f, 1.0f);
        drawSquare();
    glPopMatrix();

    // Отрисовка круга с учетом его позиции
    glPushMatrix();
        glTranslatef(circle_x, circle_y, 0.0f);
        drawCircle();
    glPopMatrix();

    glutSwapBuffers();
}

// Функция, которая обновляет состояние объектов
void idle(void) {
    // Обновляем угол поворота квадрата, если включено вращение
    if (rotating) {
        square_angle += 0.5f;
        if (square_angle > 360.0f)
            square_angle -= 360.0f;
    }

    // Обновляем позицию круга в зависимости от зажатых стрелок
    if (moveLeft)
        circle_x -= moveSpeed;
    if (moveRight)
        circle_x += moveSpeed;
    if (moveUp)
        circle_y += moveSpeed;
    if (moveDown)
        circle_y -= moveSpeed;

    glutPostRedisplay();
}

// Обработчик событий мыши
void mouse(int button, int state, int x, int y) {
    if (button == GLUT_LEFT_BUTTON && state == GLUT_DOWN) {
        rotating = true;
    } else if (button == GLUT_RIGHT_BUTTON && state == GLUT_DOWN) {
        rotating = false;
    }
}

// Обработчик нажатия специальных клавиш (стрелок)
void specialKeys(int key, int x, int y) {
    switch (key) {
        case GLUT_KEY_LEFT:  moveLeft  = true; break;
        case GLUT_KEY_RIGHT: moveRight = true; break;
        case GLUT_KEY_UP:    moveUp    = true; break;
        case GLUT_KEY_DOWN:  moveDown  = true; break;
    }
}

// Обработчик отпускания специальных клавиш (стрелок)
void specialKeysUp(int key, int x, int y) {
    switch (key) {
        case GLUT_KEY_LEFT:  moveLeft  = false; break;
        case GLUT_KEY_RIGHT: moveRight = false; break;
        case GLUT_KEY_UP:    moveUp    = false; break;
        case GLUT_KEY_DOWN:  moveDown  = false; break;
    }
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    // Используем двойную буферизацию и RGB-режим
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowSize(600, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Задание 2");

    // Регистрируем обработчики событий
    glutDisplayFunc(display);
    glutIdleFunc(idle);
    glutMouseFunc(mouse);
    glutSpecialFunc(specialKeys);
    glutSpecialUpFunc(specialKeysUp);

    // Устанавливаем белый цвет фона
    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);

    glutMainLoop();
    return 0;
}
