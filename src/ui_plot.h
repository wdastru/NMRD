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
#include <QtGui/QScrollBar>
#include <QtGui/QWidget>
#include "qcustomplot.h"

QT_BEGIN_NAMESPACE

class Ui_PlotForm
{
public:
    QCustomPlot *plotarea;
    QPushButton *pushButton;
    QScrollBar *horizontalScrollBar;
    QScrollBar *verticalScrollBar;

    void setupUi(QWidget *PlotForm)
    {
        if (PlotForm->objectName().isEmpty())
            PlotForm->setObjectName(QString::fromUtf8("PlotForm"));
        PlotForm->resize(761, 670);
        plotarea = new QCustomPlot(PlotForm);
        plotarea->setObjectName(QString::fromUtf8("plotarea"));
        plotarea->setGeometry(QRect(9, 0, 721, 581));
        pushButton = new QPushButton(PlotForm);
        pushButton->setObjectName(QString::fromUtf8("pushButton"));
        pushButton->setGeometry(QRect(10, 620, 75, 23));
        horizontalScrollBar = new QScrollBar(PlotForm);
        horizontalScrollBar->setObjectName(QString::fromUtf8("horizontalScrollBar"));
        horizontalScrollBar->setGeometry(QRect(10, 590, 721, 20));
        horizontalScrollBar->setOrientation(Qt::Horizontal);
        verticalScrollBar = new QScrollBar(PlotForm);
        verticalScrollBar->setObjectName(QString::fromUtf8("verticalScrollBar"));
        verticalScrollBar->setGeometry(QRect(730, 9, 20, 581));
        verticalScrollBar->setOrientation(Qt::Vertical);

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
