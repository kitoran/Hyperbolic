# Hyperbolic
Okay so i wanted this to be a puzzle game where puzzles are based on the fact that the space is hyperbolic but i haven't been able to come up with actual puzzles.
So here is a proof-of-concept hyperbolic space game engine prototype.

You are in hyperbolic space. Move with WASD, jump with spacebar and turn with mouse.
When you start the application there is just one red triangle snd you stand on it. You can load other environments.
If you run Hyperbolic on windows, a console window appears, you can reach it with alt-tab.
If you run Hyperbolic on linux (or probably any other OS), game console will be in terminal you started Hyperbolic from.
Commands you can run in console are:
-toggleFrame turns on/off drawing of edges of polygons.
-setGravity <number> sets gravity intensity. By default it is 0.0002
-load <path to file> loads environment from file. You can use TAB when typing path to file. There aren't any environment files in the repository at the moment. I'll probably add some.
-loadObj <path to file> [<path to file>] loads environment from .obj file. All vertices must lie inside sphere of radius 1 with center at (0, 0, 0). Since this is hyprebolic space, environment loaded from .obj file looks very different in Hyperbolic from what you would see in regular 3d editor. When two files are specified, first file is used as a mesh and second is used as physical obstacles. File used as physical obstacles cant contain any non-triangle polygons. (if only one file is specified, is is used as physical obstacles and as a mesh so it cant sontain non-triabgles too). If your .obj file is too fancy Hyperbolic might fail to parse it.
-setStep <number> specifies distance you travel when you press move keys. By default it's equal to 0.01.




Press w, s, a, d keys to move in hyperbolic space.
Move mouse to rotate viewport.
