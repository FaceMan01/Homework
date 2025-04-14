#include <GL/glut.h>
#include <cmath>

float yaw    = 1.5708f;  // Угол поворота камеры в радианах
float radius = 5.0f;     // Радиус
float eyex, eyey, eyez;  // Координаты позиции камеры


// Глобальное фоновое освещение (имитирует рассеянный свет)
GLfloat globalAmbient[4]  = { 0.2f, 0.2f, 0.2f, 1.0f };

// Параметры источника света (GL_LIGHT0)
GLfloat lightPosition[4]  = { 2.0f, 2.0f, 2.0f, 1.0f };  // Координаты света
GLfloat lightAmbient[4]   = { 0.1f, 0.1f, 0.1f, 1.0f };  // Рассеянный свет
GLfloat lightDiffuse[4]   = { 0.8f, 0.8f, 0.8f, 1.0f };  // Основное освещение
GLfloat lightSpecular[4]  = { 1.0f, 1.0f, 1.0f, 1.0f };  // Зеркальное освещение

// Параметры материалов
GLfloat matAmbient1[4]    = { 0.6f, 0.2f, 0.2f, 1.0f };
GLfloat matDiffuse1[4]    = { 0.8f, 0.0f, 0.0f, 1.0f };
GLfloat matSpecular1[4]   = { 1.0f, 1.0f, 1.0f, 1.0f };
GLfloat matShininess1[1]  = { 50.0f };

GLfloat matAmbient2[4]    = { 0.1f, 0.2f, 0.6f, 1.0f };
GLfloat matDiffuse2[4]    = { 0.2f, 0.5f, 0.8f, 1.0f };
GLfloat matSpecular2[4]   = { 0.9f, 0.9f, 0.9f, 1.0f };
GLfloat matShininess2[1]  = { 400.0f };

bool isSpotlight = true; // Изначально источник - прожектор
bool useMaterial1 = true; // Изначально используем первый набор материала

void setupLighting()
{
    // Включаем освещение
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);  // Включаем источник света 0

    // Настраиваем глобальное фоновое освещение
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, globalAmbient);

    // Устанавливаем базовые параметры источника света (GL_LIGHT0)
    glLightfv(GL_LIGHT0, GL_AMBIENT,  lightAmbient);   // Рассеянный свет
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  lightDiffuse);   // Основной свет
    glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpecular);  // Зеркальное отражение
    // Координаты источника будем обновлять в display() через glLightfv(..., GL_POSITION, ...)

    // Включаем тест глубины (чтобы ближние объекты перекрывали дальние)
    glEnable(GL_DEPTH_TEST);
    // Нормализация нормалей (чтобы освещение работало корректно при масштабировании)
    glEnable(GL_NORMALIZE);
    // Включаем сглаженное затенение (Gouraud shading)
    glShadeModel(GL_SMOOTH);
}

void applyCurrentMaterial()
{
    if (useMaterial1)
    {
        glMaterialfv(GL_FRONT, GL_AMBIENT,   matAmbient1);
        glMaterialfv(GL_FRONT, GL_DIFFUSE,   matDiffuse1);
        glMaterialfv(GL_FRONT, GL_SPECULAR,  matSpecular1);
        glMaterialfv(GL_FRONT, GL_SHININESS, matShininess1);
    }
    else
    {
        glMaterialfv(GL_FRONT, GL_AMBIENT,   matAmbient2);
        glMaterialfv(GL_FRONT, GL_DIFFUSE,   matDiffuse2);
        glMaterialfv(GL_FRONT, GL_SPECULAR,  matSpecular2);
        glMaterialfv(GL_FRONT, GL_SHININESS, matShininess2);
    }
}

// Делаем источник света прожектором или точечным (в зависимости от isSpotlight)
void setupSpotlightOrPoint()
{
    if (isSpotlight)
    {
        // Устанавливаем угол конуса света
        glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 20.0f);
        // Направление прожектора (пусть светит примерно вниз и влево)
        GLfloat spotDirection[] = { -1.0f, -1.0f, -1.0f };
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spotDirection);
        // "Резкость" света
        glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 15.0f);
    }
    else
    {
        // Точечный источник
        glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 360.0f);
        GLfloat spotDirection[] = { -1.0f, -1.0f, -1.0f };
        glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spotDirection);
        glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 0.0f);
    }
}


void display()
{
    // Очищаем буфера цвета и глубины
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();

    // Камера вращается только вокруг оси Y (yaw)
    eyex = radius * cosf(yaw);
    eyey = 0.0f;
    eyez = radius * sinf(yaw);

    // Устанавливаем позицию камеры
    gluLookAt(eyex, eyey, eyez,  // Где находится камера
              0.0f, 0.0f, 0.0f,  // Куда смотрим (центр сцены)
              0.0f, 1.0f, 0.0f); // "Вверх" - ось Y

    // Обновляем позицию света
    glLightfv(GL_LIGHT0, GL_POSITION, lightPosition);

    // Настраиваем — прожектор или нет
    setupSpotlightOrPoint();

    // Применяем текущие параметры материала
    applyCurrentMaterial();

    // Рисуем сферу (или куб — можно заменить на glutSolidCube)
    glutSolidSphere(1.0, 50, 50);

    // Переключаем буферы (двойная буферизация)
    glutSwapBuffers();
}

// Функция изменения размеров окна
void reshape(int w, int h)
{
    if (h == 0) h = 1;
    glViewport(0, 0, w, h);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    // Перспективная проекция (угол обзора 45°)
    gluPerspective(45.0, (GLfloat)w / (GLfloat)h, 0.1f, 100.0f);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

// Управление камерой и переключениями
void keyboard(unsigned char key, int x, int y)
{
    switch (key)
    {
        case 'a': // Поворот камеры влево
            yaw += 0.1f;
            break;
        case 'd': // Поворот камеры вправо
            yaw -= 0.1f;
            break;
        case 's': // Переключение "прожектор" <-> "точечный источник"
            isSpotlight = !isSpotlight;
            break;
        case 'm': // Переключение параметров материала
            useMaterial1 = !useMaterial1;
            break;
    }
    glutPostRedisplay(); // Перерисовываем
}

int main(int argc, char** argv)
{
    glutInit(&argc, argv);
    // Режим окна: RGB + буфер глубины + двойная буферизация
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);

    // Размер окна
    glutInitWindowSize(800, 600);
    glutInitWindowPosition(100, 100);
    glutCreateWindow("OpenGL Sphere with Spotlight, Toggle Light/Material");

    // Настраиваем освещение
    setupLighting();

    // Регистрируем функции обратного вызова
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);

    // Запускаем основной цикл
    glutMainLoop();
    return 0;
}
