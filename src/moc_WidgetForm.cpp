/****************************************************************************
** Meta object code from reading C++ file 'WidgetForm.h'
**
** Created by: The Qt Meta Object Compiler version 63 (Qt 4.8.6)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "WidgetForm.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'WidgetForm.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 63
#error "This file was generated using the moc from 4.8.6. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_WidgetForm[] = {

 // content:
       6,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
      12,   11,   11,   11, 0x08,
      32,   11,   11,   11, 0x08,
      66,   11,   11,   11, 0x08,
     111,   11,   11,   11, 0x08,
     127,   11,   11,   11, 0x08,
     145,   11,   11,   11, 0x08,
     165,   11,   11,   11, 0x08,
     180,   11,   11,   11, 0x08,
     198,   11,   11,   11, 0x08,
     219,   11,   11,   11, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_WidgetForm[] = {
    "WidgetForm\0\0startParaNMRD_new()\0"
    "on_datasetsSpinBox_valueChanged()\0"
    "on_TAUSComboBox_currentIndexChanged(QString)\0"
    "readInputFile()\0chooseParentDir()\0"
    "enableNewDirItems()\0addExptPoint()\0"
    "deleteExptPoint()\0setupExptPointsGui()\0"
    "printValues()\0"
};

void WidgetForm::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    if (_c == QMetaObject::InvokeMetaMethod) {
        Q_ASSERT(staticMetaObject.cast(_o));
        WidgetForm *_t = static_cast<WidgetForm *>(_o);
        switch (_id) {
        case 0: _t->startParaNMRD_new(); break;
        case 1: _t->on_datasetsSpinBox_valueChanged(); break;
        case 2: _t->on_TAUSComboBox_currentIndexChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: _t->readInputFile(); break;
        case 4: _t->chooseParentDir(); break;
        case 5: _t->enableNewDirItems(); break;
        case 6: _t->addExptPoint(); break;
        case 7: _t->deleteExptPoint(); break;
        case 8: _t->setupExptPointsGui(); break;
        case 9: _t->printValues(); break;
        default: ;
        }
    }
}

const QMetaObjectExtraData WidgetForm::staticMetaObjectExtraData = {
    0,  qt_static_metacall 
};

const QMetaObject WidgetForm::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_WidgetForm,
      qt_meta_data_WidgetForm, &staticMetaObjectExtraData }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &WidgetForm::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *WidgetForm::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *WidgetForm::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_WidgetForm))
        return static_cast<void*>(const_cast< WidgetForm*>(this));
    return QWidget::qt_metacast(_clname);
}

int WidgetForm::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < 10)
            qt_static_metacall(this, _c, _id, _a);
        _id -= 10;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
