#include <GL/freeglut.h>
#include <math.h>

#define PI 3.14159265f

// Параметры камеры
float cameraAngle = 0.0f;  // Угол вращения камеры
float cameraHeight = 2.0f; // Высота камеры
float radius = 8.0f;       // Радиус вращения камеры вокруг сферы

// Параметры освещения
GLfloat globalAmbient[] = { 0.2f, 0.2f, 0.2f, 1.0f };
GLfloat lightPos[] = { 0.0f, 5.0f, 5.0f, 1.0f };
GLfloat spotDir[] = { 0.0f, -1.0f, -1.0f };
GLfloat lightAmbient[] = { 0.1f, 0.1f, 0.1f, 1.0f };
GLfloat lightDiffuse[] = { 0.8f, 0.8f, 0.8f, 1.0f };
GLfloat lightSpecular[] = { 1.0f, 1.0f, 1.0f, 1.0f };

// Параметры материала шара
GLfloat matAmbient[] = { 0.2f, 0.0f, 0.0f, 1.0f };
GLfloat matDiffuse[] = { 0.8f, 0.0f, 0.0f, 1.0f };
GLfloat matSpecular[] = { 1.0f, 1.0f, 1.0f, 1.0f };
GLfloat matShininess[] = { 50.0f };

void initLighting(void) {
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, globalAmbient);
    glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
    glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmbient);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiffuse);
    glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpecular);
    glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 30.0f);
    glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spotDir);
    glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 20.0f);
}

void initMaterial(void) {
    glMaterialfv(GL_FRONT, GL_AMBIENT, matAmbient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, matDiffuse);
    glMaterialfv(GL_FRONT, GL_SPECULAR, matSpecular);
    glMaterialfv(GL_FRONT, GL_SHININESS, matShininess);
}

void init(void) {
    glEnable(GL_DEPTH_TEST);
    initLighting();
    initMaterial();
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
}

void display(void) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
    float camX = radius * sin(cameraAngle);
    float camZ = radius * cos(cameraAngle);
    
    gluLookAt(camX, cameraHeight, camZ, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);
    
    glLightfv(GL_LIGHT0, GL_POSITION, lightPos);
    glutSolidSphere(1.5, 50, 50);
    glutSwapBuffers();
}

void reshape(int w, int h) {
    if (h == 0) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45.0, (float)w/(float)h, 1.0, 100.0);
}

void keyboard(unsigned char key, int x, int y) {
    switch (key) {
        case 'a': cameraAngle -= 0.1f; break;
        case 'd': cameraAngle += 0.1f; break;
        case 'w': cameraHeight += 0.5f; break;
        case 's': cameraHeight -= 0.5f; break;
    }
    glutPostRedisplay();
}

int main(int argc, char** argv) {
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowSize(800, 600);
    glutCreateWindow("Камера и освещение");
    
    init();
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard);
    
    glutMainLoop();
    return 0;
}
