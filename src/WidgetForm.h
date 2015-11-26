/*
 * widgetform.h
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#ifndef WIDGETFORM_H_
#define WIDGETFORM_H_

#include <string>
#include <QtCore/QString>

#include "ui_mainwidget.h"

class WidgetForm: public QWidget {
Q_OBJECT

public:
	WidgetForm(QWidget *parent = 0);

private slots:
	void startParaNMRD();
	void startParaNMRD_new();
	void on_datasetsSpinBox_valueChanged();
	void readInputFile();

private:
	Ui::WidgetForm ui;
	QString inputFilename;
	QString outputFilename;
	void writeInputFile();
};

#endif /* WIDGETFORM_H_ */
