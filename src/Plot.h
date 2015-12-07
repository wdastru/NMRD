/*
 * Plot.h
 *
 *  Created on: 06/dic/2015
 *      Author: Wax
 */

#ifndef PLOT_H_
#define PLOT_H_

#include "ui_plot.h"

class PlotForm: public QWidget {
Q_OBJECT

friend class WidgetForm;

public:
	PlotForm(QWidget *parent = 0);

private slots:

private:
	Ui::PlotForm ui;
};

#endif /* PLOT_H_ */
