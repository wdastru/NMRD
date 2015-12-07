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
	void startParaNMRD();
	void startParaNMRD_new();
	void on_datasetsSpinBox_valueChanged();
	void readInputFile();
	void chooseParentDir();
	void enableNewDirItems();
	void addExptPoint();
	void deleteExptPoint();
	void setupExptPointsGui();
	//void setupExptPoints();

private:
	Ui::WidgetForm ui;
	QString inputFilename;
	QString outputFilename;
	void writeInputFile();
	QString dir;
	QFileInfo inputfileInfo;
	PlotForm plot;
	QVector<QHBoxLayout*> exptPointLayout;
};

#endif /* WIDGETFORM_H_ */
