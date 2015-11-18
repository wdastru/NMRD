/********************************************************************************
** Form generated from reading UI file 'mainwidget.ui'
**
** Created by: Qt User Interface Compiler version 4.8.6
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWIDGET_H
#define UI_MAINWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDoubleSpinBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpinBox>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_WidgetForm
{
public:
    QPushButton *startButton;
    QPushButton *closeButton;
    QWidget *layoutWidget;
    QGridLayout *gridLayout;
    QLabel *inputFileLabel;
    QLineEdit *inputFileLineEdit;
    QLabel *outputFileLabel;
    QLineEdit *outputFileLineEdit;
    QLabel *metalNuclearSpinLabel;
    QDoubleSpinBox *metalNuclearSpinDoubleSpinBox;
    QLabel *gammaILabel;
    QDoubleSpinBox *gammaIDoubleSpinBox;
    QSpinBox *gammaIExpSpinBox;
    QLabel *gammaILabel2;
    QLabel *elSpinLabel;
    QDoubleSpinBox *elSpinDoubleSpinBox;

    void setupUi(QWidget *WidgetForm)
    {
        if (WidgetForm->objectName().isEmpty())
            WidgetForm->setObjectName(QString::fromUtf8("WidgetForm"));
        WidgetForm->setWindowModality(Qt::NonModal);
        WidgetForm->resize(400, 630);
        startButton = new QPushButton(WidgetForm);
        startButton->setObjectName(QString::fromUtf8("startButton"));
        startButton->setGeometry(QRect(20, 590, 75, 23));
        closeButton = new QPushButton(WidgetForm);
        closeButton->setObjectName(QString::fromUtf8("closeButton"));
        closeButton->setGeometry(QRect(310, 590, 75, 23));
        layoutWidget = new QWidget(WidgetForm);
        layoutWidget->setObjectName(QString::fromUtf8("layoutWidget"));
        layoutWidget->setGeometry(QRect(20, 20, 361, 126));
        gridLayout = new QGridLayout(layoutWidget);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setContentsMargins(0, 0, 0, 0);
        inputFileLabel = new QLabel(layoutWidget);
        inputFileLabel->setObjectName(QString::fromUtf8("inputFileLabel"));

        gridLayout->addWidget(inputFileLabel, 0, 0, 1, 1);

        inputFileLineEdit = new QLineEdit(layoutWidget);
        inputFileLineEdit->setObjectName(QString::fromUtf8("inputFileLineEdit"));

        gridLayout->addWidget(inputFileLineEdit, 0, 1, 1, 1);

        outputFileLabel = new QLabel(layoutWidget);
        outputFileLabel->setObjectName(QString::fromUtf8("outputFileLabel"));

        gridLayout->addWidget(outputFileLabel, 1, 0, 1, 1);

        outputFileLineEdit = new QLineEdit(layoutWidget);
        outputFileLineEdit->setObjectName(QString::fromUtf8("outputFileLineEdit"));

        gridLayout->addWidget(outputFileLineEdit, 1, 1, 1, 1);

        metalNuclearSpinLabel = new QLabel(layoutWidget);
        metalNuclearSpinLabel->setObjectName(QString::fromUtf8("metalNuclearSpinLabel"));

        gridLayout->addWidget(metalNuclearSpinLabel, 2, 0, 1, 1);

        metalNuclearSpinDoubleSpinBox = new QDoubleSpinBox(layoutWidget);
        metalNuclearSpinDoubleSpinBox->setObjectName(QString::fromUtf8("metalNuclearSpinDoubleSpinBox"));
        metalNuclearSpinDoubleSpinBox->setDecimals(1);
        metalNuclearSpinDoubleSpinBox->setMinimum(-100);
        metalNuclearSpinDoubleSpinBox->setSingleStep(0.5);

        gridLayout->addWidget(metalNuclearSpinDoubleSpinBox, 2, 1, 1, 1);

        gammaILabel = new QLabel(layoutWidget);
        gammaILabel->setObjectName(QString::fromUtf8("gammaILabel"));

        gridLayout->addWidget(gammaILabel, 3, 0, 1, 1);

        gammaIDoubleSpinBox = new QDoubleSpinBox(layoutWidget);
        gammaIDoubleSpinBox->setObjectName(QString::fromUtf8("gammaIDoubleSpinBox"));
        gammaIDoubleSpinBox->setDecimals(6);
        gammaIDoubleSpinBox->setMaximum(10);
        gammaIDoubleSpinBox->setSingleStep(1e-06);
        gammaIDoubleSpinBox->setValue(2.6752);

        gridLayout->addWidget(gammaIDoubleSpinBox, 3, 1, 1, 1);

        gammaIExpSpinBox = new QSpinBox(layoutWidget);
        gammaIExpSpinBox->setObjectName(QString::fromUtf8("gammaIExpSpinBox"));
        gammaIExpSpinBox->setMinimum(0);
        gammaIExpSpinBox->setValue(8);

        gridLayout->addWidget(gammaIExpSpinBox, 3, 3, 1, 1);

        gammaILabel2 = new QLabel(layoutWidget);
        gammaILabel2->setObjectName(QString::fromUtf8("gammaILabel2"));

        gridLayout->addWidget(gammaILabel2, 3, 2, 1, 1);

        elSpinLabel = new QLabel(layoutWidget);
        elSpinLabel->setObjectName(QString::fromUtf8("elSpinLabel"));

        gridLayout->addWidget(elSpinLabel, 4, 0, 1, 1);

        elSpinDoubleSpinBox = new QDoubleSpinBox(layoutWidget);
        elSpinDoubleSpinBox->setObjectName(QString::fromUtf8("elSpinDoubleSpinBox"));
        elSpinDoubleSpinBox->setDecimals(1);
        elSpinDoubleSpinBox->setSingleStep(0.5);
        elSpinDoubleSpinBox->setValue(3.5);

        gridLayout->addWidget(elSpinDoubleSpinBox, 4, 1, 1, 1);

        QWidget::setTabOrder(inputFileLineEdit, outputFileLineEdit);
        QWidget::setTabOrder(outputFileLineEdit, startButton);
        QWidget::setTabOrder(startButton, closeButton);

        retranslateUi(WidgetForm);
        QObject::connect(closeButton, SIGNAL(clicked(bool)), WidgetForm, SLOT(close()));
        QObject::connect(startButton, SIGNAL(clicked(bool)), WidgetForm, SLOT(startParaNMRD()));
        QObject::connect(inputFileLineEdit, SIGNAL(textChanged(QString)), WidgetForm, SLOT(setInputFilename()));
        QObject::connect(outputFileLineEdit, SIGNAL(textChanged(QString)), WidgetForm, SLOT(setOutputFilename()));
        QObject::connect(metalNuclearSpinDoubleSpinBox, SIGNAL(valueChanged(double)), WidgetForm, SLOT(setMetalNuclearSpin()));

        QMetaObject::connectSlotsByName(WidgetForm);
    } // setupUi

    void retranslateUi(QWidget *WidgetForm)
    {
        WidgetForm->setWindowTitle(QApplication::translate("WidgetForm", "NMRD", 0, QApplication::UnicodeUTF8));
        startButton->setText(QApplication::translate("WidgetForm", "START", 0, QApplication::UnicodeUTF8));
        closeButton->setText(QApplication::translate("WidgetForm", "CLOSE", 0, QApplication::UnicodeUTF8));
        inputFileLabel->setText(QApplication::translate("WidgetForm", "input file:", 0, QApplication::UnicodeUTF8));
        outputFileLabel->setText(QApplication::translate("WidgetForm", "output file:", 0, QApplication::UnicodeUTF8));
        metalNuclearSpinLabel->setText(QApplication::translate("WidgetForm", "metal nuclear spin", 0, QApplication::UnicodeUTF8));
        gammaILabel->setText(QApplication::translate("WidgetForm", "gammaI of investigated particle", 0, QApplication::UnicodeUTF8));
        gammaILabel2->setText(QApplication::translate("WidgetForm", "10^", 0, QApplication::UnicodeUTF8));
        elSpinLabel->setText(QApplication::translate("WidgetForm", "electron spin", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class WidgetForm: public Ui_WidgetForm {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWIDGET_H
