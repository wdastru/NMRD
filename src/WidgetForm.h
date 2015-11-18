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
	void setInputFilename();
	void setOutputFilename();
	void setMetalNuclearSpin();
	void setGammaI();

private:
	Ui::WidgetForm ui;
	QString inputFilename;
	QString outputFilename;
	double metalNuclearSpin;
	//float gammaI;

};

#endif /* WIDGETFORM_H_ */
