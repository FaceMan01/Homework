#include <GL/freeglut.h>
#include <math.h>
#include <stdlib.h>

#define PI 3.14159265358979323846

// Глобальные переменные для управления камерой
float radius = 5.0f; // Радиус вращения камеры вокруг куба
float angle = 0.0f;  // Угол поворота камеры вокруг куба (в градусах)
float eyex, eyez;    // Координаты камеры по X и Z (вычисляются)
float eyey = 1.5f;   // Высота камеры

// Флаг проекции: 1 - перспективная, 0 - параллельная
int perspective = 1;

// Функция обновления позиции камеры
void updateCameraPosition() {
    eyex = radius * cos(angle * PI / 180.0); // Вычисляем X по углу
    eyez = radius * sin(angle * PI / 180.0); // Вычисляем Z по углу
}

void drawCube() {
    glBegin(GL_QUADS);

    glColor3f(1, 0, 0);
    glVertex3f(-1, -1, 1); glVertex3f(1, -1, 1);
    glVertex3f(1, 1, 1); glVertex3f(-1, 1, 1);
    
    glColor3f(0, 1, 0);
    glVertex3f(-1, -1, -1); glVertex3f(-1, 1, -1);
    glVertex3f(1, 1, -1); glVertex3f(1, -1, -1);
    
    glColor3f(0, 0, 1);
    glVertex3f(-1, -1, -1); glVertex3f(-1, -1, 1);
    glVertex3f(-1, 1, 1); glVertex3f(-1, 1, -1);
    
    glColor3f(1, 1, 0);
    glVertex3f(1, -1, -1); glVertex3f(1, 1, -1);
    glVertex3f(1, 1, 1); glVertex3f(1, -1, 1);
    
    glColor3f(1, 0, 1);
    glVertex3f(-1, 1, -1); glVertex3f(-1, 1, 1);
    glVertex3f(1, 1, 1); glVertex3f(1, 1, -1);
    
    glColor3f(0, 1, 1);
    glVertex3f(-1, -1, -1); glVertex3f(1, -1, -1);
    glVertex3f(1, -1, 1); glVertex3f(-1, -1, 1);
    
    glEnd();
}

// Функция инициализации OpenGL
void init(void) {
    glEnable(GL_DEPTH_TEST); // Включаем тест глубины
    glClearColor(0.0, 0.0, 0.0, 1.0); // Чёрный фон
    updateCameraPosition(); // Вычисляем начальную позицию камеры
}

// Функция отрисовки сцены
void display(void) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    // Устанавливаем камеру: gluLookAt(eyex, eyey, eyez, 0, 0, 0, 0, 1, 0)
    gluLookAt(eyex, eyey, eyez, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);

    // Рисуем куб
    drawCube();

    glutSwapBuffers();
}

// Функция обработки изменения размеров окна
void reshape(int w, int h) {
    if (h == 0) h = 1;
    float aspect = (float)w / (float)h;

    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    if (perspective) {
        gluPerspective(60.0, aspect, 1.0, 100.0);
    } else {
        float orthoSize = 2.5;
        glOrtho(-orthoSize * aspect, orthoSize * aspect, -orthoSize, orthoSize, 1.0, 100.0);
    }

    glMatrixMode(GL_MODELVIEW);
}

// Функция обработки нажатия клавиш
void keyboard(unsigned char key, int x, int y) {
    float step = 0.2f; // Шаг перемещения камеры
    switch (key) {
        case 'd': // Поворот камеры влево (по часовой стрелке)
            angle -= step;
            break;
        case 'a': // Поворот камеры вправо (против часовой стрелки)
            angle += step;
            break;
        case 'w': // Подъем камеры
            eyey += step;
            break;
        case 's': // Опускание камеры
            eyey -= step;
            break;
        case 'q': // Приближение камеры (уменьшение радиуса)
            radius -= step;
            if (radius < 1.0f) radius = 1.0f;
            break;
        case 'e': // Отдаление камеры (увеличение радиуса)
            radius += step;
            if (radius > 10.0f) radius = 10.0f;
            break;
        case 'p': // Переключение проекции
            perspective = !perspective;
            reshape(glutGet(GLUT_WINDOW_WIDTH), glutGet(GLUT_WINDOW_HEIGHT));
            break;
        case 27: // Выход (Esc)
            exit(0);
            break;
    }

    updateCameraPosition(); // Пересчитываем координаты камеры
    glutPostRedisplay();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(800, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("Управление камерой вокруг куба");

    init();

    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);

    glutMainLoop();
    return 0;
}
