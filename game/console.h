#ifndef CONSOLE_H
#define CONSOLE_H
#include <list>
#include <string>

const int historySize = 30;
void addToHistory(const std::string& line);
void loadHistory();
extern const std::list<std::string> &history;
inline std::list<std::string>::const_reverse_iterator
        positionInHistory;
inline std::string console;
inline std::string savedConsole;

void processLine();

#endif // CONSOLE_H
