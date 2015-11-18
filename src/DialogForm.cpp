/*
 * DialogForm.cpp
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#include <iostream>

#include <QtGui>

#include "DialogForm.h"

extern "C" {
	void paranmrd_();
}

DialogForm::DialogForm(QDialog *parent) :
	QDialog(parent) {
	ui.setupUi(this);
}

void DialogForm::startParaNMRD() {
	paranmrd_();
}
