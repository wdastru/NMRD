/*
 * WidgetForm.cpp
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#include <iostream>
#include <math.h>

#include <QtGui/QDoubleSpinBox>
#include <QtGui/QComboBox>

#include "WidgetForm.h"

extern "C" {
	void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *, double *, double *, \
			unsigned int *, double *, double *, double *, unsigned int *);
}

WidgetForm::WidgetForm(QWidget *parent) :
	QWidget(parent) {
	ui.setupUi(this);
	inputFilename = "PARC_NEW.DAT";
	outputFilename = "PARC.OUT";
	ui.inputFileLineEdit->setText(inputFilename);
	ui.outputFileLineEdit->setText(outputFilename);
}

void WidgetForm::startParaNMRD() {

	QByteArray ba1 = ui.inputFileLineEdit->text().toLatin1();
	QByteArray ba2 = ui.outputFileLineEdit->text().toLatin1();

	char *inputFN = ba1.data();
	char *outputFN = ba2.data();

	unsigned int inputLen = strlen(inputFN);
	unsigned int outputLen = strlen(outputFN);

	unsigned int max = (inputLen>outputLen) ? inputLen : outputLen;

	double metalNuclearSpin = ui.metalNuclearSpinDoubleSpinBox->value();

	double gammaI = ui.gammaIDoubleSpinBox->value()*pow(10,ui.gammaIExpSpinBox->value());
	double elSpin = ui.elSpinDoubleSpinBox->value();

	unsigned int T1T2;
	if (ui.T1T2ComboBox->currentText() == "T1") {
		T1T2 = 1;
	} else if (ui.T1T2ComboBox->currentText() == "T1") {
		T1T2 = 2;
	}

	double X1 = ui.fieldRangeX1DoubleSpinBox->value();
	double X2 = ui.fieldRangeX2DoubleSpinBox->value();
	double X3 = ui.fieldRangeX3DoubleSpinBox->value();

	unsigned int np = ui.numberOfPointsSpinBox->value();

	paranmrd_(inputFN, &max, outputFN, &max, &metalNuclearSpin, &gammaI, &elSpin, &T1T2, \
			&X1, &X2, &X3, &np);
}
