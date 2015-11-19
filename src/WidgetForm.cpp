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
void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *,
		double *, double *, unsigned int *, double *, double *, double *,
		unsigned int *, double *, double *, unsigned int *);
}

WidgetForm::WidgetForm(QWidget *parent) :
		QWidget(parent) {
	ui.setupUi(this);
	inputFilename = "PARC_NEW.DAT";
	outputFilename = "PARC.OUT";
	ui.inputFileLineEdit->setText(inputFilename);
	ui.outputFileLineEdit->setText(outputFilename);
}

void WidgetForm::enableTempSpinBoxes() {
	if (ui.datasetsSpinBox->value() == 1) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(false);
		ui.temp3DoubleSpinBox->setEnabled(false);
	} else if (ui.datasetsSpinBox->value() == 2) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(true);
		ui.temp3DoubleSpinBox->setEnabled(false);
	} else if (ui.datasetsSpinBox->value() == 3) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(true);
		ui.temp3DoubleSpinBox->setEnabled(true);
	}
}

void WidgetForm::startParaNMRD() {

	QByteArray ba1 = ui.inputFileLineEdit->text().toLatin1();
	QByteArray ba2 = ui.outputFileLineEdit->text().toLatin1();

	char *inputFN = ba1.data();
	char *outputFN = ba2.data();

	unsigned int inputLen = strlen(inputFN);
	unsigned int outputLen = strlen(outputFN);

	unsigned int max = (inputLen > outputLen) ? inputLen : outputLen;

	double metalNuclearSpin = ui.metalNuclearSpinDoubleSpinBox->value();

	double gammaI = ui.gammaIDoubleSpinBox->value()
			* pow(10, ui.gammaIExpSpinBox->value());
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

	/*
	 * ds sono i dataset che dovranno essere trattati
	 * dsDouble e' servito perche' se passavo un in
	 */
	unsigned int ds = ui.datasetsSpinBox->value();
	double dsDouble = (double) ds;

	double temp[3] = {273, 310, 298};

	//double fpA[]={1.2f,3.f,44.f,2.5f,-1.3f,33.44f,5.f,0.3f,-3.6f,24.1f};

	paranmrd_(inputFN, &max, outputFN, &max, &metalNuclearSpin, &gammaI,
			&elSpin, &T1T2, &X1, &X2, &X3, &np, &dsDouble, temp, &ds);
}
