#ifndef PERSISTENTLINEEDIT_H
#define PERSISTENTLINEEDIT_H

#include <QLineEdit>
#include <QSettings>

class PersistentLineEdit : public QLineEdit {
public:
    PersistentLineEdit(QWidget* w=nullptr) : QLineEdit(w) {
        connect(this, &PersistentLineEdit::objectNameChanged,
                this, [this](QString f){
            setText(QSettings().value(f).toString());
        });
    }
    ~PersistentLineEdit() {
        QSettings().setValue(objectName(), text());
    }

};

#endif // PERSISTENTLINEEDIT_H
