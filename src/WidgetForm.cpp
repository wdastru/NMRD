/*
 * WidgetForm.cpp
 *
 *  Created on: 14/nov/2015
 *      Author: Wax
 */

#include <iostream>
#include <fstream>

#include <math.h>
#include <string.h>
#include <unistd.h>

#include <QtGui/QDoubleSpinBox>
#include <QtGui/QComboBox>
#include <QtCore/QByteArray>
#include <QtCore/QRegExp>
#include <QtGui/QFileDialog>

#include "WidgetForm.h"
#include "qcustomplot.h"

#define FIXED(param) param->isChecked() ? file << 0 << ' ' : file << 1 << ' ';
#define COUT(something) std::cout << something << std::endl;

using namespace std;

extern "C" {
//void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *,\
		double *, double *, unsigned int *, double *, double *, double *,\
		unsigned int *, double *, double *, unsigned int *);
//void paranmrdorig_(char *, unsigned int *, char *, unsigned int *, char *, unsigned int *);
void paranmrdorig_(char *, char *, char *);
}

WidgetForm::WidgetForm(QWidget *parent) :
	QWidget(parent) {
	ui.setupUi(this);
	inputFilename = "";
	outputFilename = "";
	parametersFilename = "";
	ui.inputFileLineEdit->setText(inputFilename);
	ui.xyFileLineEdit->setText(outputFilename);
	ui.parsOutFileLineEdit->setText(parametersFilename);

	x_max = x_min = y_max = y_min = 0;

	enableNewDirItems();
}

void WidgetForm::on_datasetsSpinBox_valueChanged() {
	if (ui.datasetsSpinBox->value() == 1) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(false);
		ui.temp3DoubleSpinBox->setEnabled(false);
		ui.nExpPts1SpinBox->setEnabled(true);
		ui.nExpPts2SpinBox->setEnabled(false);
		ui.nExpPts3SpinBox->setEnabled(false);
	} else if (ui.datasetsSpinBox->value() == 2) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(true);
		ui.temp3DoubleSpinBox->setEnabled(false);
		ui.nExpPts1SpinBox->setEnabled(true);
		ui.nExpPts2SpinBox->setEnabled(true);
		ui.nExpPts3SpinBox->setEnabled(false);
	} else if (ui.datasetsSpinBox->value() == 3) {
		ui.temp1DoubleSpinBox->setEnabled(true);
		ui.temp2DoubleSpinBox->setEnabled(true);
		ui.temp3DoubleSpinBox->setEnabled(true);
		ui.nExpPts1SpinBox->setEnabled(true);
		ui.nExpPts2SpinBox->setEnabled(true);
		ui.nExpPts3SpinBox->setEnabled(true);
	}
}

void WidgetForm::writeInputFile() {

#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	std::string str = this->dir.toStdString() + '\\'
			+ this->inputfileInfo.fileName().toStdString();

	ofstream file;

	file.open(str.c_str());

	file << ui.xyFileLineEdit->text().toStdString().c_str() << std::endl;
	file << ui.parsOutFileLineEdit->text().toStdString().c_str() << std::endl;
	file << ui.metalNuclearSpinDoubleSpinBox->value() << std::endl;
	file << ui.gammaIDoubleSpinBox->value() * pow(10,
			ui.gammaIExpSpinBox->value()) << std::endl;
	file << ui.elSpinDoubleSpinBox->value() << std::endl;

	if (ui.T1T2ComboBox->currentText() == "T1") {
		file << 1 << std::endl;
	} else if (ui.T1T2ComboBox->currentText() == "T1") {
		file << 2 << std::endl;
	}

	file << ui.fieldRangeX1DoubleSpinBox->value() << ' '
			<< ui.fieldRangeX2DoubleSpinBox->value() << ' '
			<< ui.fieldRangeX3DoubleSpinBox->value() << std::endl;
	file << ui.numberOfPointsSpinBox->value() << std::endl;
	file << ui.datasetsSpinBox->value() << std::endl;

	if (ui.datasetsSpinBox->value() == 1) {
		file << ui.temp1DoubleSpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 2) {
		file << ui.temp1DoubleSpinBox->value() << ' '
				<< ui.temp2DoubleSpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 3) {
		file << ui.temp1DoubleSpinBox->value() << ' '
				<< ui.temp2DoubleSpinBox->value() << ' '
				<< ui.temp3DoubleSpinBox->value() << std::endl;
	}

	FIXED(ui.TauSCheckBox);
	file << ui.TAUMS01DoubleSpinBox->value() * pow(10,
			ui.TAUMS01ExpSpinBox->value()) << ' ';
	file << ui.TAUMS02DoubleSpinBox->value() * pow(10,
			ui.TAUMS02ExpSpinBox->value()) << ' ';
	file << ui.TAUDELTADoubleSpinBox->value() * pow(10,
			ui.TAUDELTAExpSpinBox->value()) << std::endl;

	FIXED(ui.TauRCheckBox);
	file << ui.TAURM1DoubleSpinBox->value() * pow(10,
			ui.TAURM1ExpSpinBox->value()) << ' ';
	file << ui.TAURM2DoubleSpinBox->value() * pow(10,
			ui.TAURM2ExpSpinBox->value()) << std::endl;

	FIXED(ui.TauVCheckBox);
	file << ui.TAUVM1DoubleSpinBox->value() * pow(10,
			ui.TAUVM1ExpSpinBox->value()) << ' ';
	file << ui.TAUVM2DoubleSpinBox->value() * pow(10,
			ui.TAUVM2ExpSpinBox->value()) << std::endl;

	FIXED(ui.DZFSCheckBox);
	file << ui.DZFSDoubleSpinBox->value() * pow(10, ui.DZFSExpSpinBox->value())
			<< std::endl;

	FIXED(ui.EZFSCheckBox);
	file << ui.EZFSDoubleSpinBox->value() * pow(10, ui.EZFSExpSpinBox->value())
			<< std::endl;

	FIXED(ui.S4MCheckBox);
	file << ui.S4MDoubleSpinBox->value() * pow(10, ui.S4MExpSpinBox->value())
			<< std::endl;

	FIXED(ui.gxCheckBox);
	file << ui.gxDoubleSpinBox->value() * pow(10, ui.gxExpSpinBox->value())
			<< std::endl;

	FIXED(ui.gyCheckBox);
	file << ui.gyDoubleSpinBox->value() * pow(10, ui.gyExpSpinBox->value())
			<< std::endl;

	FIXED(ui.gzCheckBox);
	file << ui.gzDoubleSpinBox->value() * pow(10, ui.gzExpSpinBox->value())
			<< std::endl;

	FIXED(ui.AxCheckBox);
	file << ui.AxDoubleSpinBox->value() * pow(10, ui.AxExpSpinBox->value())
			<< std::endl;

	FIXED(ui.AyCheckBox);
	file << ui.AyDoubleSpinBox->value() * pow(10, ui.AyExpSpinBox->value())
			<< std::endl;

	FIXED(ui.AzCheckBox);
	file << ui.AzDoubleSpinBox->value() * pow(10, ui.AzExpSpinBox->value())
			<< std::endl;

	FIXED(ui.distanceCheckBox);
	file << ui.distanceDoubleSpinBox->value() * pow(10,
			ui.distanceExpSpinBox->value()) << std::endl;

	FIXED(ui.DCoeffCheckBox);
	file << ui.DCoeffDoubleSpinBox->value() * pow(10,
			ui.DCoeffExpSpinBox->value()) << std::endl;

	FIXED(ui.concCheckBox);
	file << ui.concDoubleSpinBox->value() * pow(10, ui.concExpSpinBox->value())
			<< std::endl;

	file << ui.typesOfWaterSpinBox->value() << std::endl;

	FIXED(ui.taumCheckBox);
	file << ui.taum1DoubleSpinBox->value() * pow(10,
			ui.taum1ExpSpinBox->value()) << ' ';
	file << ui.taum2DoubleSpinBox->value() * pow(10,
			ui.taum2ExpSpinBox->value()) << std::endl;

	FIXED(ui.molFracCheckBox);
	file << ui.molFracDoubleSpinBox->value() * pow(10,
			ui.molFracExpSpinBox->value()) << std::endl;

	FIXED(ui.rkDistanceCheckBox);
	file << ui.rkDistanceDoubleSpinBox->value() * pow(10,
			ui.rkDistanceExpSpinBox->value()) << std::endl;

	FIXED(ui.AHCheckBox);
	file << ui.AHDoubleSpinBox->value() * pow(10, ui.AHExpSpinBox->value())
			<< std::endl;

	FIXED(ui.thetaCheckBox);
	file << ui.thetaDoubleSpinBox->value() << std::endl;

	FIXED(ui.phiCheckBox);
	file << ui.phiDoubleSpinBox->value() << std::endl;

	if (ui.datasetsSpinBox->value() == 1) {
		file << ui.nExpPts1SpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 2) {
		file << ui.nExpPts1SpinBox->value() << ' '
				<< ui.nExpPts2SpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 3) {
		file << ui.nExpPts1SpinBox->value() << ' '
				<< ui.nExpPts2SpinBox->value() << ' '
				<< ui.nExpPts3SpinBox->value() << std::endl;
	}

	file << ui.toleranceDoubleSpinBox->value() * pow(10,
			ui.toleranceExpSpinBox->value()) << std::endl;

	file << ui.fittingStepDoubleSpinBox->value() << std::endl;

	for (unsigned int i=0; i<exptPointXDoubleSpinBoxes.size(); i++) {
		file << exptPointXDoubleSpinBoxes.at(i)->value() << ' ' << exptPointYDoubleSpinBoxes.at(i)->value() << std::endl;
	}

	file.close();
}

void WidgetForm::chooseParentDir() {
#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	this->dir = QFileDialog::getExistingDirectory(this, tr("Open Directory"),
			this->dir,
			QFileDialog::ShowDirsOnly | QFileDialog::DontResolveSymlinks);
	ui.newDirLineEdit->setText(this->dir);

	//COUT(FUNCTION_NAME << this->dir.toStdString());
}

void WidgetForm::enableNewDirItems() {
	if (ui.newDirCheckBox->isChecked()) {
		ui.newDirLineEdit->setEnabled(true);
		ui.chooseParentDirPushButton->setEnabled(true);
	} else {
		ui.newDirLineEdit->setEnabled(false);
		ui.chooseParentDirPushButton->setEnabled(false);
	}
}

void WidgetForm::readInputFile() {

#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	this->inputFilename = QFileDialog::getOpenFileName(this,
			tr("Open Input File"), ".", tr("Text Files (*.txt *.dat)"));

	this->inputfileInfo.setFile(this->inputFilename);

	this->dir = inputfileInfo.absoluteDir().absolutePath();

	//COUT(FUNCTION_NAME << this->dir.toStdString().c_str());

	ui.inputFileLineEdit->setText(inputfileInfo.fileName());

	// input.txt has integers, one per line
	ifstream file(inputfileInfo.absoluteFilePath().toStdString().c_str());

	std::string str;
	int i;
	double d;
	QStringList items;

	file >> str;
	ui.xyFileLineEdit->setText(QString::fromStdString(str));

	file >> str;
	ui.parsOutFileLineEdit->setText(QString::fromStdString(str));

	file >> d;
	ui.metalNuclearSpinDoubleSpinBox->setValue(d);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.gammaIDoubleSpinBox->setValue(items[0].toDouble());
		ui.gammaIExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.gammaIDoubleSpinBox->setValue(items[0].toDouble());
		ui.gammaIExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> d;
	ui.elSpinDoubleSpinBox->setValue(d);

	file >> i;
	if (i == 1 || i == 2) {
		ui.T1T2ComboBox->setCurrentIndex(i - 1);
	} else {
		COUT("ERROR! T1/T2 parameter illegal value!");
	}

	file >> d;
	ui.fieldRangeX1DoubleSpinBox->setValue(d);
	file >> d;
	ui.fieldRangeX2DoubleSpinBox->setValue(d);
	file >> d;
	ui.fieldRangeX3DoubleSpinBox->setValue(d);

	file >> i;
	ui.numberOfPointsSpinBox->setValue(i);

	file >> i;
	int ds = i;
	ui.datasetsSpinBox->setValue(ds);

	if (ds == 1) {
		file >> d;
		ui.temp1DoubleSpinBox->setValue(d);
	} else if (ds == 2) {
		file >> d;
		ui.temp1DoubleSpinBox->setValue(d);
		file >> d;
		ui.temp2DoubleSpinBox->setValue(d);
	} else if (ds == 3) {
		file >> d;
		ui.temp1DoubleSpinBox->setValue(d);
		file >> d;
		ui.temp2DoubleSpinBox->setValue(d);
		file >> d;
		ui.temp3DoubleSpinBox->setValue(d);
	}

	file >> i;
	i == 0 ? ui.TauSCheckBox->setChecked(true) : ui.TauSCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAUMS01DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUMS01ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAUMS01DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUMS01ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAUMS02DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUMS02ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAUMS02DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUMS02ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAUDELTADoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUDELTAExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAUDELTADoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUDELTAExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.TauRCheckBox->setChecked(true) : ui.TauRCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAURM1DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAURM1ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAURM1DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAURM1ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAURM2DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAURM2ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAURM2DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAURM2ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.TauVCheckBox->setChecked(true) : ui.TauVCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAUVM1DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUVM1ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAUVM1DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUVM1ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.TAUVM2DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUVM2ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.TAUVM2DoubleSpinBox->setValue(items[0].toDouble());
		ui.TAUVM2ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.DZFSCheckBox->setChecked(true) : ui.DZFSCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.DZFSDoubleSpinBox->setValue(items[0].toDouble());
		ui.DZFSExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.DZFSDoubleSpinBox->setValue(items[0].toDouble());
		ui.DZFSExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.EZFSCheckBox->setChecked(true) : ui.EZFSCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.EZFSDoubleSpinBox->setValue(items[0].toDouble());
		ui.EZFSExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.EZFSDoubleSpinBox->setValue(items[0].toDouble());
		ui.EZFSExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.S4MCheckBox->setChecked(true) : ui.S4MCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.S4MDoubleSpinBox->setValue(items[0].toDouble());
		ui.S4MExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.S4MDoubleSpinBox->setValue(items[0].toDouble());
		ui.S4MExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.gxCheckBox->setChecked(true) : ui.gxCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.gxDoubleSpinBox->setValue(items[0].toDouble());
		ui.gxExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.gxDoubleSpinBox->setValue(items[0].toDouble());
		ui.gxExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.gyCheckBox->setChecked(true) : ui.gyCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.gyDoubleSpinBox->setValue(items[0].toDouble());
		ui.gyExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.gyDoubleSpinBox->setValue(items[0].toDouble());
		ui.gyExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.gzCheckBox->setChecked(true) : ui.gzCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.gzDoubleSpinBox->setValue(items[0].toDouble());
		ui.gzExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.gzDoubleSpinBox->setValue(items[0].toDouble());
		ui.gzExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.AxCheckBox->setChecked(true) : ui.AxCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.AxDoubleSpinBox->setValue(items[0].toDouble());
		ui.AxExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.AxDoubleSpinBox->setValue(items[0].toDouble());
		ui.AxExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.AyCheckBox->setChecked(true) : ui.AyCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.AyDoubleSpinBox->setValue(items[0].toDouble());
		ui.AyExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.AyDoubleSpinBox->setValue(items[0].toDouble());
		ui.AyExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.AzCheckBox->setChecked(true) : ui.AzCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.AzDoubleSpinBox->setValue(items[0].toDouble());
		ui.AzExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.AzDoubleSpinBox->setValue(items[0].toDouble());
		ui.AzExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.distanceCheckBox->setChecked(true)
			: ui.distanceCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.distanceDoubleSpinBox->setValue(items[0].toDouble());
		ui.distanceExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.distanceDoubleSpinBox->setValue(items[0].toDouble());
		ui.distanceExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.DCoeffCheckBox->setChecked(true)
			: ui.DCoeffCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.DCoeffDoubleSpinBox->setValue(items[0].toDouble());
		ui.DCoeffExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.DCoeffDoubleSpinBox->setValue(items[0].toDouble());
		ui.DCoeffExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.concCheckBox->setChecked(true) : ui.concCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.concDoubleSpinBox->setValue(items[0].toDouble());
		ui.concExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.concDoubleSpinBox->setValue(items[0].toDouble());
		ui.concExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	ui.typesOfWaterSpinBox->setValue(i);

	file >> i;
	i == 0 ? ui.taumCheckBox->setChecked(true) : ui.taumCheckBox->setChecked(
			false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.taum1DoubleSpinBox->setValue(items[0].toDouble());
		ui.taum1ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.taum1DoubleSpinBox->setValue(items[0].toDouble());
		ui.taum1ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.taum2DoubleSpinBox->setValue(items[0].toDouble());
		ui.taum2ExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.taum2DoubleSpinBox->setValue(items[0].toDouble());
		ui.taum2ExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.molFracCheckBox->setChecked(true)
			: ui.molFracCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.molFracDoubleSpinBox->setValue(items[0].toDouble());
		ui.molFracExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.molFracDoubleSpinBox->setValue(items[0].toDouble());
		ui.molFracExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.rkDistanceCheckBox->setChecked(true)
			: ui.rkDistanceCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.rkDistanceDoubleSpinBox->setValue(items[0].toDouble());
		ui.rkDistanceExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.rkDistanceDoubleSpinBox->setValue(items[0].toDouble());
		ui.rkDistanceExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.AHCheckBox->setChecked(true) : ui.AHCheckBox->setChecked(false);

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.AHDoubleSpinBox->setValue(items[0].toDouble());
		ui.AHExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.AHDoubleSpinBox->setValue(items[0].toDouble());
		ui.AHExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> i;
	i == 0 ? ui.thetaCheckBox->setChecked(true) : ui.thetaCheckBox->setChecked(
			false);

	file >> d;
	ui.thetaDoubleSpinBox->setValue(d);

	file >> i;
	i == 0 ? ui.phiCheckBox->setChecked(true) : ui.phiCheckBox->setChecked(
			false);

	file >> d;
	ui.phiDoubleSpinBox->setValue(d);

	if (ds == 1) {
		file >> i;
		ui.nExpPts1SpinBox->setValue(i);
	} else if (ds == 2) {
		file >> i;
		ui.nExpPts1SpinBox->setValue(i);
		file >> i;
		ui.nExpPts2SpinBox->setValue(i);
	} else if (ds == 3) {
		file >> i;
		ui.nExpPts1SpinBox->setValue(i);
		file >> i;
		ui.nExpPts2SpinBox->setValue(i);
		file >> i;
		ui.nExpPts3SpinBox->setValue(i);
	}

	setupExptPointsGui();

	file >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"),
			QString::SkipEmptyParts);
	if (items.size() == 1) {
		ui.toleranceDoubleSpinBox->setValue(items[0].toDouble());
		ui.toleranceExpSpinBox->setValue(0);
	} else if (items.size() == 2) {
		ui.toleranceDoubleSpinBox->setValue(items[0].toDouble());
		ui.toleranceExpSpinBox->setValue(items[1].toInt());
	} else {
		/* TO DO
		 * catch error
		 */
	}

	file >> d;
	ui.fittingStepDoubleSpinBox->setValue(d);

	for (unsigned int i = 0; i < ui.nExpPts1SpinBox->value(); i++) {
		file >> d;
		exptPointXDoubleSpinBoxes.at(i)->setValue(d);
		file >> d;
		exptPointYDoubleSpinBoxes.at(i)->setValue(d);
	}

//	printValues();

	plot.show();

	/* experimental points */
	plot.ui.plotarea->addGraph();
	plot.ui.plotarea->graph()->setPen(QPen(Qt::red));
	plot.ui.plotarea->graph()->setLineStyle(QCPGraph::lsNone);
	plot.ui.plotarea->graph()->setScatterStyle(QCPScatterStyle::ssDisc);
	QVector<double> exptX, exptY;
	for (unsigned int i=0; i<exptPointXDoubleSpinBoxes.size(); i++) {

		double x_val = exptPointXDoubleSpinBoxes.at(i)->value();
		double y_val = exptPointYDoubleSpinBoxes.at(i)->value();

		exptX.push_back(x_val);
		exptY.push_back(y_val);

		x_max = x_val > x_max ? x_val : x_max;
		x_min = x_val < x_min ? x_val : x_min;
		y_max = y_val > y_max ? y_val : y_max;
		y_min = y_val < y_min ? y_val : y_min;

	}
	plot.ui.plotarea->graph()->setData(exptX, exptY);
	plot.ui.plotarea->xAxis->setRange(x_min, x_max);
	plot.ui.plotarea->yAxis->setRange(y_min, y_max);
	plot.ui.plotarea->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom);
	plot.ui.plotarea->axisRect()->setupFullAxesBox(true);
	plot.ui.plotarea->xAxis->setScaleType(QCPAxis::stLogarithmic);
	plot.ui.plotarea->xAxis->setScaleLogBase(100);
	plot.ui.plotarea->replot();

}

void WidgetForm::startParaNMRD_new() {

#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	if (!ui.newDirLineEdit->text().isEmpty()) {
		this->dir = ui.newDirLineEdit->text();
	}

	chdir(this->dir.toStdString().c_str());

	//COUT(this->dir.toStdString().c_str());

	writeInputFile();

	QByteArray ba1 = ui.inputFileLineEdit->text().toLatin1();
	QByteArray ba2 = ui.xyFileLineEdit->text().toLatin1();
	QByteArray ba3 = ui.parsOutFileLineEdit->text().toLatin1();

	char *inputFN = ba1.data();
	char *xyFN = ba2.data();
	char *parsFN = ba3.data();

	unsigned int inputLen = strlen(inputFN);
	unsigned int xyLen = strlen(xyFN);
	unsigned int parsLen = strlen(parsFN);

	unsigned int max = (inputLen > xyLen) ? inputLen : xyLen;

	//paranmrdorig_(inputFN, &max, xyFN, &max, parsFN, &max);
	paranmrdorig_(inputFN, xyFN, parsFN);
	
	/* fitted data */
	plot.ui.plotarea->addGraph();
	plot.ui.plotarea->graph()->setPen(QPen(Qt::blue));
	plot.ui.plotarea->graph()->setBrush(QBrush(QColor(0, 0, 255, 20)));
	//	plot.ui.plotarea->addGraph();
	//	plot.ui.plotarea->graph(1)->setPen(QPen(Qt::red));

	ifstream file(ui.xyFileLineEdit->text().toStdString().c_str());

	QVector<double> x, y;
	double x_val, y_val;

	while (file >> x_val && file >> y_val) {

		x_max = x_val > x_max ? x_val : x_max;
		x_min = x_val < x_min ? x_val : x_min;
		y_max = y_val > y_max ? y_val : y_max;
		y_min = y_val < y_min ? y_val : y_min;

		//COUT(x_val << '\t' << y_val);

		x.push_back(x_val);
		y.push_back(y_val);
	}

	file.close();

	plot.ui.horizontalScrollBar->setRange((int) x_min * 100, (int) x_max * 100);
	plot.ui.verticalScrollBar->setRange((int) y_min * 100, (int) y_max * 100);
	plot.ui.plotarea->graph()->setData(x, y);

	plot.ui.plotarea->xAxis->setRange(x_min, x_max);
	plot.ui.plotarea->yAxis->setRange(y_min, y_max);

	plot.ui.plotarea->replot();
}

void WidgetForm::printValues() {
	for (unsigned int i=0; i<exptPointXDoubleSpinBoxes.size(); i++) {
		COUT(i << '\t' << exptPointXDoubleSpinBoxes.at(i)->value() << '\t' << exptPointYDoubleSpinBoxes.at(i)->value());
	}
	COUT("----------------------------------------------------");
}

void WidgetForm::addExptPoint() {
#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	QDoubleSpinBox *dsbx = new QDoubleSpinBox();
	dsbx->setDecimals(5);
	dsbx->setMaximum(99);
	dsbx->setSingleStep(0.1);
//	QObject::connect(dsbx, SIGNAL(valueChanged(double)), this, SLOT(printValues()));

	QDoubleSpinBox *dsby = new QDoubleSpinBox();
	dsby->setDecimals(5);
	dsby->setMaximum(99);
	dsby->setSingleStep(0.1);
//	QObject::connect(dsby, SIGNAL(valueChanged(double)), this, SLOT(printValues()));

	exptPointXDoubleSpinBoxes.push_back(dsbx);
	exptPointYDoubleSpinBoxes.push_back(dsby);

	QHBoxLayout* layout = new QHBoxLayout();
	layout->addWidget(exptPointXDoubleSpinBoxes.at(exptPointXDoubleSpinBoxes.size()-1));
	layout->addWidget(exptPointYDoubleSpinBoxes.at(exptPointYDoubleSpinBoxes.size()-1));

	ui.exptPointsVLayout->addLayout(layout);

//	printValues();

}

void WidgetForm::deleteExptPoint() {
#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	delete ui.exptPointsVLayout->takeAt(0);
	exptPointXDoubleSpinBoxes.pop_front();
	exptPointYDoubleSpinBoxes.pop_front();

//	printValues();

}

void WidgetForm::setupExptPointsGui() {
#undef FUNCTION_NAME
#define FUNCTION_NAME __FILE__ << ":" << __FUNCTION__ << ":" << __LINE__ << ". "

	int
			toAdd =
					(ui.nExpPts1SpinBox->value()
							> ui.exptPointsVLayout->count()) ? ui.nExpPts1SpinBox->value()
							- ui.exptPointsVLayout->count()
							: 0;

	for (unsigned int i = 0; i < toAdd; i++)
		addExptPoint(); // because of the label field/R1

	int
			toDelete =
					(ui.nExpPts1SpinBox->value()
							< ui.exptPointsVLayout->count()) ? ui.exptPointsVLayout->count()
							- ui.nExpPts1SpinBox->value()
							: 0;

	for (unsigned int i = 0; i < toDelete; i++)
		deleteExptPoint(); // delete the last row

//	printValues();

}

//void WidgetForm::startParaNMRD() {
//
//	QByteArray ba1 = ui.inputFileLineEdit->text().toLatin1();
//	QByteArray ba2 = ui.xyFileLineEdit->text().toLatin1();
//
//	char *inputFN = ba1.data();
//	char *xyFN = ba2.data();
//
//	unsigned int inputLen = strlen(inputFN);
//	unsigned int xyLen = strlen(xyFN);
//
//	unsigned int max = (inputLen > xyLen) ? inputLen : xyLen;
//
//	double metalNuclearSpin = ui.metalNuclearSpinDoubleSpinBox->value();
//
//	double gammaI = ui.gammaIDoubleSpinBox->value() * pow(10,
//			ui.gammaIExpSpinBox->value());
//	double elSpin = ui.elSpinDoubleSpinBox->value();
//
//	unsigned int T1T2;
//	if (ui.T1T2ComboBox->currentText() == "T1") {
//		T1T2 = 1;
//	} else if (ui.T1T2ComboBox->currentText() == "T1") {
//		T1T2 = 2;
//	}
//
//	double X1 = ui.fieldRangeX1DoubleSpinBox->value();
//	double X2 = ui.fieldRangeX2DoubleSpinBox->value();
//	double X3 = ui.fieldRangeX3DoubleSpinBox->value();
//
//	unsigned int np = ui.numberOfPointsSpinBox->value();
//
//	/*
//	 * ds sono i dataset che dovranno essere trattati
//	 * dsDouble e' servito perche' se passavo un int
//	 * il fortran crasha
//	 */
//	unsigned int ds = ui.datasetsSpinBox->value();
//	double dsDouble = (double) ds;
//
//	double temp[3] = { 273, 310, 298 };
//
//	//double fpA[]={1.2f,3.f,44.f,2.5f,-1.3f,33.44f,5.f,0.3f,-3.6f,24.1f};
//
//	//paranmrd_(inputFN, &max, xyFN, &max, &metalNuclearSpin, &gammaI,\
//			&elSpin, &T1T2, &X1, &X2, &X3, &np, &dsDouble, temp, &ds);
//}
