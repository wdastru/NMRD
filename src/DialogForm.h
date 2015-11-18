/*
 * dialogform.h
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#ifndef DIALOGFORM_H_
#define DIALOGFORM_H_

#include "ui_maindialog.h"

class DialogForm: public QDialog {
Q_OBJECT

public:
	DialogForm(QDialog *parent = 0);

private slots:
	void startParaNMRD();

private:
	Ui::DialogForm ui;

};

#endif /* DIALOGFORM_H_ */
