#include "editor.h"

//int findSelected(WorldState ws, AvatarPosition ap) {

//    getObject()


//    return foldMaybesP( ws.devis, (/*{-transposeMink- $-}*/ G::viewPort(ap)));
//    // -- тут мы много раз считаем преобразование, а надо один если вообще считать
//}

//void gameLoop() {
//    glEnable(GL_DEPTH);
//    glEnable(GL_TEXTURE_2D);
//    for(const Source& s : level().sources) {

//        Mesh sm = G::sourceMesh();

//        sm = moveRightTo(s.p) * sm;


//        levelMesh.insert(levelMesh.end(), sm.begin(), sm.end());
//    }
//    uint a = SDL_GetTicks();
//    int cycles = 0;
//    bool focus = true;
//    int mouseInhibited = 0;
//    while(continueCycle) {
//        SDL_Event event;
//        if (SDL_PollEvent(&event)) {
//            if(event.type == SDL_WINDOWEVENT) {
//                if(event.window.event == SDL_WINDOWEVENT_FOCUS_GAINED) focus = true;
//                else if(event.window.event == SDL_WINDOWEVENT_FOCUS_LOST) focus = false;
//            } else if(event.type == SDL_QUIT) {
//                continueCycle = false;
//            }
//        }
//        if(focus) {
//            int x ,y;
//            uint res = SDL_GetMouseState(&x, &y);
//            if(x != width/2 || y != height/2) {
////                state.avatarPosition = processMouse( x-width/2, y-height/2, state.avatarPosition);
//                state.selected = state.inventory.type == Item::Empty ? findSelected(state.worldState, state.avatarPosition) : boost::none;
////                SDL_WarpMouseInWindow(window, width/2, height/2);
//            }
//            if(res &  SDL_BUTTON(SDL_BUTTON_LEFT) && !mouseInhibited) {
//                mouseCCase();
//                mouseInhibited = 6;
//            }
//            if(mouseInhibited>0) mouseInhibited--;

//            //
//            keyboardProcess();
//            processTimer();
//            //Console_Draw(tty);       SDL_GL_SwapWindow(window);
//            gameDisplay();
//            //            SDL_FlushEvents(SDL_QUIT+1, SDL_LASTEVENT);
//            cycles++;
//            uint newa = SDL_GetTicks();
//            if(newa - a >= 1000) {
//                std::cerr << "кадров за прошедшую секунду" << cycles << std::endl;
//                a = newa;
//                cycles = 0;
//            }
//        } else {
//            SDL_GL_SwapWindow(window);
//        }
//    }

//}
