/********************************************************************************
** Form generated from reading UI file 'plot.ui'
**
** Created by: Qt User Interface Compiler version 4.8.6
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PLOT_H
#define UI_PLOT_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QWidget>
#include "qcustomplot.h"

QT_BEGIN_NAMESPACE

class Ui_PlotForm
{
public:
    QCustomPlot *plotarea;
    QPushButton *pushButton;

    void setupUi(QWidget *PlotForm)
    {
        if (PlotForm->objectName().isEmpty())
            PlotForm->setObjectName(QString::fromUtf8("PlotForm"));
        PlotForm->resize(744, 649);
        plotarea = new QCustomPlot(PlotForm);
        plotarea->setObjectName(QString::fromUtf8("plotarea"));
        plotarea->setGeometry(QRect(9, 9, 721, 591));
        pushButton = new QPushButton(PlotForm);
        pushButton->setObjectName(QString::fromUtf8("pushButton"));
        pushButton->setGeometry(QRect(20, 610, 75, 23));

        retranslateUi(PlotForm);
        QObject::connect(pushButton, SIGNAL(clicked()), PlotForm, SLOT(close()));

        QMetaObject::connectSlotsByName(PlotForm);
    } // setupUi

    void retranslateUi(QWidget *PlotForm)
    {
        PlotForm->setWindowTitle(QApplication::translate("PlotForm", "Plot", 0, QApplication::UnicodeUTF8));
        pushButton->setText(QApplication::translate("PlotForm", "Close", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class PlotForm: public Ui_PlotForm {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PLOT_H
