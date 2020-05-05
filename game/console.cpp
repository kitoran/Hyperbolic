#include "console.h"
#include "gameloop.h"

std::list<std::string> p_history = {};
const std::list<std::string>& history = p_history;

void addToHistory(const std::string &line) {
    p_history.push_back(line);
    if(p_history.size() > historySize) {
        p_history.pop_front();
    }

    FILE* f = fopen("consoleHistory.txt", "a");
    fprintf(f, "%s\n", line.c_str());
    fclose(f);
}

//const std::list<std::string> &history()
//{
//    return history;
//}

void loadHistory()
{
    FILE* f = fopen("consoleHistory.txt", "r");
    if(f == NULL) {
        return;
    }
    char * line = NULL;
    size_t s = 0;
    while(getline(&line, &s, f) > 0) {
        p_history.push_back(std::string(line, strlen(line)-1));
        free(line);
        line = NULL;
    }
    fclose(f);
    while(p_history.size() > historySize) {
        p_history.pop_back();
    }
    positionInHistory = history.rend();
}

void processLine() {
    if(console == "getde") {
        ::state.inventory.type = Item::De;
        ::state.inventory.size = 0.005;
    }
//    if(console == "editor") {
//        editorLoop();
//    }
    if(console == "noclip") {
        p_history.push_back("noclip toggled");
        noclip = !noclip;
    }
}


