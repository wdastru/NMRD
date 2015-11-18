#include <QtGui/QApplication>

#include "WidgetForm.h"

#include <iostream>

using namespace std;

int main(int argv, char **args) {

	QApplication app(argv, args);
	WidgetForm widgetform;
	widgetform.show();
	return app.exec();
}
