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
#include <QtCore/QFileInfo>

#include "ui_mainwidget.h"
#include "Plot.h"

class WidgetForm: public QWidget {
Q_OBJECT

public:
	WidgetForm(QWidget *parent = 0);

private slots:
	//void startParaNMRD();
	void startParaNMRD_new();
	void on_datasetsSpinBox_valueChanged();
	void readInputFile();
	void chooseParentDir();
	void enableNewDirItems();
	void addExptPoint();
	void deleteExptPoint();
	void setupExptPointsGui();
	void printValues();
	//void setupExptPoints();

private:
	Ui::WidgetForm ui;
	QString inputFilename;
	QString outputFilename;
	QString parametersFilename;
	void writeInputFile();
	QString dir;
	QFileInfo inputfileInfo;
	PlotForm plot;
	QVector<QDoubleSpinBox*> exptPointXDoubleSpinBoxes;
	QVector<QDoubleSpinBox*> exptPointYDoubleSpinBoxes;
	double x_max, x_min, y_max, y_min;
};

#endif /* WIDGETFORM_H_ */
