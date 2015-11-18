/*
 * WidgetForm.cpp
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#include <iostream>
#include <math.h>

#include <QtGui/QDoubleSpinBox>

#include "WidgetForm.h"

extern "C" {
	//void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *, float *);
	void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *);
}

WidgetForm::WidgetForm(QWidget *parent) :
	QWidget(parent) {
	ui.setupUi(this);
	inputFilename = "PARC_NEW.DAT";
	outputFilename = "PARC.OUT";
	ui.inputFileLineEdit->setText(inputFilename);
	ui.outputFileLineEdit->setText(outputFilename);

	//while (1) {
	//	ui.startButton->click();
	//}
}

void WidgetForm::setInputFilename() {
	this->inputFilename = ui.inputFileLineEdit->text();
}

void WidgetForm::setOutputFilename() {
	this->outputFilename = ui.outputFileLineEdit->text();
}

void WidgetForm::setMetalNuclearSpin() {
	this->metalNuclearSpin = ui.metalNuclearSpinDoubleSpinBox->value();
}

void WidgetForm::setGammaI() {
	//this->gammaI = ui.gammaIDoubleSpinBox->value()*pow(10,ui.gammaIExpSpinBox->value());
}

void WidgetForm::startParaNMRD() {

	QByteArray ba1 = this->inputFilename.toLatin1();
	QByteArray ba2 = this->outputFilename.toLatin1();

	char *inputFN = ba1.data();
	char *outputFN = ba2.data();

	unsigned int inputLen = strlen(inputFN);
	unsigned int outputLen = strlen(outputFN);

	unsigned int max = (inputLen>outputLen) ? inputLen : outputLen;

	//paranmrd_(inputFN, &max, outputFN, &max, &this->metalNuclearSpin, &this->gammaI);
	paranmrd_(inputFN, &max, outputFN, &max, &this->metalNuclearSpin);
}
