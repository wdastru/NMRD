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

#include <QtGui/QDoubleSpinBox>
#include <QtGui/QComboBox>
#include <QtCore/QByteArray>
#include <QtCore/QRegExp.h>

#include "WidgetForm.h"

#define FIXED(param) param->isChecked() ? outFile << 0 << ' ' : outFile << 1 << ' ';

using namespace std;

extern "C" {
//void paranmrd_(char *, unsigned int *, char *, unsigned int *, double *,\
		double *, double *, unsigned int *, double *, double *, double *,\
		unsigned int *, double *, double *, unsigned int *);
void paranmrdorig_(char *, unsigned int *, char *, unsigned int *);
}

WidgetForm::WidgetForm(QWidget *parent) :
		QWidget(parent) {
	ui.setupUi(this);
	//inputFilename = "PARC_NEW.DAT";
	inputFilename = "PARC_NEW_NEW.DAT";
	outputFilename = "PARC.OUT";
	ui.inputFileLineEdit->setText(inputFilename);
	ui.outputFileLineEdit->setText(outputFilename);
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
	ofstream outFile;
	outFile.open("PARC_NEW_NEW.DAT");

	outFile << ui.outputFileLineEdit->text().toStdString().c_str() << std::endl;
	outFile << ui.metalNuclearSpinDoubleSpinBox->value() << std::endl;
	outFile
			<< ui.gammaIDoubleSpinBox->value()
					* pow(10, ui.gammaIExpSpinBox->value()) << std::endl;
	outFile << ui.elSpinDoubleSpinBox->value() << std::endl;

	if (ui.T1T2ComboBox->currentText() == "T1") {
		outFile << 1 << std::endl;
	} else if (ui.T1T2ComboBox->currentText() == "T1") {
		outFile << 2 << std::endl;
	}

	outFile << ui.fieldRangeX1DoubleSpinBox->value() << ' '
			<< ui.fieldRangeX2DoubleSpinBox->value() << ' '
			<< ui.fieldRangeX3DoubleSpinBox->value() << std::endl;
	outFile << ui.numberOfPointsSpinBox->value() << std::endl;
	outFile << ui.datasetsSpinBox->value() << std::endl;

	if (ui.datasetsSpinBox->value() == 1) {
		outFile << ui.temp1DoubleSpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 2) {
		outFile << ui.temp1DoubleSpinBox->value() << ' '
				<< ui.temp2DoubleSpinBox->value() << std::endl;
	} else if (ui.datasetsSpinBox->value() == 3) {
		outFile << ui.temp1DoubleSpinBox->value() << ' '
				<< ui.temp2DoubleSpinBox->value() << ' '
				<< ui.temp3DoubleSpinBox->value() << std::endl;
	}

	FIXED(ui.TauSCheckBox);
	outFile << ui.TAUMS01DoubleSpinBox->value() * pow(10, ui.TAUMS01ExpSpinBox->value()) << ' ';
	outFile << ui.TAUMS02DoubleSpinBox->value() * pow(10, ui.TAUMS02ExpSpinBox->value()) << ' ';
	outFile << ui.TAUDELTADoubleSpinBox->value() * pow(10, ui.TAUDELTAExpSpinBox->value()) << std::endl;

	FIXED(ui.TauRCheckBox);
	outFile << ui.TAURM1DoubleSpinBox->value() * pow(10, ui.TAURM1ExpSpinBox->value()) << ' ';
	outFile << ui.TAURM2DoubleSpinBox->value() * pow(10, ui.TAURM2ExpSpinBox->value()) << std::endl;

	FIXED(ui.TauVCheckBox);
	outFile << ui.TAUVM1DoubleSpinBox->value() * pow(10, ui.TAUVM1ExpSpinBox->value()) << ' ';
	outFile << ui.TAUVM2DoubleSpinBox->value() * pow(10, ui.TAUVM2ExpSpinBox->value()) << std::endl;

	FIXED(ui.DZFSCheckBox);
	outFile << ui.DZFSDoubleSpinBox->value() * pow(10, ui.DZFSExpSpinBox->value()) << std::endl;

	FIXED(ui.EZFSCheckBox);
	outFile << ui.EZFSDoubleSpinBox->value() * pow(10, ui.EZFSExpSpinBox->value()) << std::endl;

	FIXED(ui.S4MCheckBox);
	outFile << ui.S4MDoubleSpinBox->value() * pow(10, ui.S4MExpSpinBox->value()) << std::endl;

	FIXED(ui.gxCheckBox);
	outFile << ui.gxDoubleSpinBox->value() * pow(10, ui.gxExpSpinBox->value()) << std::endl;

	FIXED(ui.gyCheckBox);
	outFile << ui.gyDoubleSpinBox->value() * pow(10, ui.gyExpSpinBox->value()) << std::endl;

	FIXED(ui.gzCheckBox);
	outFile << ui.gzDoubleSpinBox->value() * pow(10, ui.gzExpSpinBox->value()) << std::endl;

	FIXED(ui.AxCheckBox);
	outFile << ui.AxDoubleSpinBox->value() * pow(10, ui.AxExpSpinBox->value()) << std::endl;

	FIXED(ui.AyCheckBox);
	outFile << ui.AyDoubleSpinBox->value() * pow(10, ui.AyExpSpinBox->value()) << std::endl;

	FIXED(ui.AzCheckBox);
	outFile << ui.AzDoubleSpinBox->value() * pow(10, ui.AzExpSpinBox->value()) << std::endl;

	FIXED(ui.distanceCheckBox);
	outFile << ui.distanceDoubleSpinBox->value() * pow(10, ui.distanceExpSpinBox->value()) << std::endl;

	FIXED(ui.DCoeffCheckBox);
	outFile << ui.DCoeffDoubleSpinBox->value() * pow(10, ui.DCoeffExpSpinBox->value()) << std::endl;

	FIXED(ui.concCheckBox);
	outFile << ui.concDoubleSpinBox->value() * pow(10, ui.concExpSpinBox->value()) << std::endl;

	outFile << ui.typesOfWaterSpinBox->value() << std::endl;

	FIXED(ui.taumCheckBox);
	outFile << ui.taum1DoubleSpinBox->value() * pow(10, ui.taum1ExpSpinBox->value()) << ' ';
	outFile << ui.taum2DoubleSpinBox->value() * pow(10, ui.taum2ExpSpinBox->value()) << std::endl;

	FIXED(ui.molFracCheckBox);
	outFile << ui.molFracDoubleSpinBox->value() * pow(10, ui.molFracExpSpinBox->value()) << std::endl;

	FIXED(ui.rkDistanceCheckBox);
	outFile << ui.rkDistanceDoubleSpinBox->value() * pow(10, ui.rkDistanceExpSpinBox->value()) << std::endl;

	FIXED(ui.AHCheckBox);
	outFile << ui.AHDoubleSpinBox->value() * pow(10, ui.AHExpSpinBox->value()) << std::endl;

	FIXED(ui.thetaCheckBox);
	outFile << ui.thetaDoubleSpinBox->value() << std::endl;

	FIXED(ui.phiCheckBox);
	outFile << ui.phiDoubleSpinBox->value() << std::endl;

	if (ui.datasetsSpinBox->value() == 1) {
			outFile << ui.nExpPts1SpinBox->value() << std::endl;
		} else if (ui.datasetsSpinBox->value() == 2) {
			outFile << ui.nExpPts1SpinBox->value() << ' '
					<< ui.nExpPts2SpinBox->value() << std::endl;
		} else if (ui.datasetsSpinBox->value() == 3) {
			outFile << ui.nExpPts1SpinBox->value() << ' '
					<< ui.nExpPts2SpinBox->value() << ' '
					<< ui.nExpPts3SpinBox->value() << std::endl;
		}

	outFile << ui.toleranceDoubleSpinBox->value() * pow(10, ui.toleranceExpSpinBox->value()) << std::endl;

	outFile << ui.fittingStepDoubleSpinBox->value() << std::endl;

	outFile.close();
}

void WidgetForm::readInputFile() {

	std::cout << "----" << std::endl << "In readInputFile() : " << ui.inputFileLineEdit->text().toStdString()<< std::endl;

	ifstream inFile(ui.inputFileLineEdit->text().toStdString().c_str());	// input.txt has integers, one per line

	std::string str;
	int i;
	double d;
	QStringList items;

	inFile >> str;
	ui.outputFileLineEdit->setText(QString::fromStdString(str));

	inFile >> d;
	ui.metalNuclearSpinDoubleSpinBox->setValue(d);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> d;
	ui.elSpinDoubleSpinBox->setValue(d);

	inFile >> i;
	if (i==1 || i==2) {
		ui.T1T2ComboBox->setCurrentIndex(i-1);
	} else {
		std::cout << "ERROR! T1/T2 parameter illegal value!" << std::endl;
	}

	inFile >> d;
	ui.fieldRangeX1DoubleSpinBox->setValue(d);
	inFile >> d;
	ui.fieldRangeX2DoubleSpinBox->setValue(d);
	inFile >> d;
	ui.fieldRangeX3DoubleSpinBox->setValue(d);

	inFile >> i;
	ui.numberOfPointsSpinBox->setValue(i);
	//ui.datasetsSpinBox->setValue(i);
	//ui.typesOfWaterSpinBox->setValue(i);

	inFile >> i;
	int ds = i;
	ui.datasetsSpinBox->setValue(ds);

	if (ds==1) {
		inFile >> d;
		ui.temp1DoubleSpinBox->setValue(d);
	} else if (ds==2) {
		inFile >> d;
		ui.temp1DoubleSpinBox->setValue(d);
		inFile >> d;
		ui.temp2DoubleSpinBox->setValue(d);
	} else if (ds==3) {
		inFile >> d;
		ui.temp1DoubleSpinBox->setValue(d);
		inFile >> d;
		ui.temp2DoubleSpinBox->setValue(d);
		inFile >> d;
		ui.temp3DoubleSpinBox->setValue(d);
	}

	inFile >> i;
	i==0 ? ui.TauSCheckBox->setChecked(true) : ui.TauSCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.TauRCheckBox->setChecked(true) : ui.TauRCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.TauVCheckBox->setChecked(true) : ui.TauVCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.DZFSCheckBox->setChecked(true) : ui.DZFSCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.EZFSCheckBox->setChecked(true) : ui.EZFSCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.S4MCheckBox->setChecked(true) : ui.S4MCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.gxCheckBox->setChecked(true) : ui.gxCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.gyCheckBox->setChecked(true) : ui.gyCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.gzCheckBox->setChecked(true) : ui.gzCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.AxCheckBox->setChecked(true) : ui.AxCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.AyCheckBox->setChecked(true) : ui.AyCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.AzCheckBox->setChecked(true) : ui.AzCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.distanceCheckBox->setChecked(true) : ui.distanceCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.DCoeffCheckBox->setChecked(true) : ui.DCoeffCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.concCheckBox->setChecked(true) : ui.concCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	ui.typesOfWaterSpinBox->setValue(i);

	inFile >> i;
	i==0 ? ui.taumCheckBox->setChecked(true) : ui.taumCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.molFracCheckBox->setChecked(true) : ui.molFracCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.rkDistanceCheckBox->setChecked(true) : ui.rkDistanceCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.AHCheckBox->setChecked(true) : ui.AHCheckBox->setChecked(false);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> i;
	i==0 ? ui.thetaCheckBox->setChecked(true) : ui.thetaCheckBox->setChecked(false);

	inFile >> d;
	ui.thetaDoubleSpinBox->setValue(d);

	inFile >> i;
	i==0 ? ui.phiCheckBox->setChecked(true) : ui.phiCheckBox->setChecked(false);

	inFile >> d;
	ui.phiDoubleSpinBox->setValue(d);

	inFile >> i;
	ui.numberOfPointsSpinBox->setValue(i);

	inFile >> str;
	items.clear();
	items = QString::fromStdString(str).split(QRegExp("[eD+]"), QString::SkipEmptyParts);
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

	inFile >> d;
	ui.fittingStepDoubleSpinBox->setValue(d);

}

void WidgetForm::startParaNMRD_new() {

	writeInputFile();

	QByteArray ba1 = ui.inputFileLineEdit->text().toLatin1();
	QByteArray ba2 = ui.outputFileLineEdit->text().toLatin1();

	char *inputFN = ba1.data();
	char *outputFN = ba2.data();

	unsigned int inputLen = strlen(inputFN);
	unsigned int outputLen = strlen(outputFN);

	unsigned int max = (inputLen > outputLen) ? inputLen : outputLen;

	paranmrdorig_(inputFN, &max, outputFN, &max);
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
	 * dsDouble e' servito perche' se passavo un int
	 * il fortran crasha
	 */
	unsigned int ds = ui.datasetsSpinBox->value();
	double dsDouble = (double) ds;

	double temp[3] = { 273, 310, 298 };

	//double fpA[]={1.2f,3.f,44.f,2.5f,-1.3f,33.44f,5.f,0.3f,-3.6f,24.1f};

	//paranmrd_(inputFN, &max, outputFN, &max, &metalNuclearSpin, &gammaI,\
			&elSpin, &T1T2, &X1, &X2, &X3, &np, &dsDouble, temp, &ds);
}
