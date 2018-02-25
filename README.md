# Hyperbolic
Okay so i wanted this to be a puzzle game where puzzles are based on the fact that the space is hyperbolic but i haven't been able to come up with actual puzzles.
So here is a proof-of-concept hyperbolic space game engine prototype.

You are in hyperbolic space. Move with WASD, jump with spacebar and turn with mouse. There are also keys z and c to move up and down.
When you start the application there is just one red triangle snd you stand on it. You can load other environments.
If you run Hyperbolic on windows, a console window appears, you can reach it with alt-tab.
If you run Hyperbolic on linux (or probably any other OS), game console will be in terminal you started Hyperbolic from.

Commands you can run in console are:
- toggleFrame turns on/off drawing of edges of polygons.
- setGravity \<number> sets gravity intensity. By default it is 0.0002
- load \<path to file> loads environment from file. You can use TAB when typing path to file. There aren't any environment files in the repository at the moment. I'll probably add some.
- loadObj \<path to file> [\<path to file>] loads environment from .obj file. All vertices must lie inside sphere of radius 1 with center at (0, 0, 0). Since this is hyprebolic space, environment loaded from .obj file looks very different in Hyperbolic from what you would see in regular 3d editor. When two files are specified, first file is used as a mesh and second is used as physical obstacles. File used as physical obstacles cant contain any non-triangle polygons. (if only one file is specified, is is used as physical obstacles and as a mesh so it cant sontain non-triabgles too). If your .obj file is too fancy Hyperbolic might fail to parse it.
 - setStep \<number> specifies distance you travel when you press move keys. By default it's equal to 0.01.
 - setJump \<number> sets velocity you are given when you press spacebar. Default is 0.01.
- quit closes the program. Program may crash when closing or freeglut can complain about something.
- help prints list of commands and short descriptions.
- window turns off fulscreen mode.
- fullscreen does what you expect it to do.
- state prints some information about your position and other stuff in incomprehensible format. It's basically useless, much like the Hyperbolic itself.

Gravity gets very weak when you are far from horizontal plane z=0. Because hyperbolic space. So if you jump when on high altitude, you may never fall down and fly endlessly in infinite dark void. To prevent this there is invisible black ceiling that will revert your vertical speed when you bump into it.

If you move far away from start point strange things can happen - the space can collapse into one point or line. Fixing this would require some knowledge in group theory which i don't have. 

Collision detection is very inefficient because it's not obvious how to make analog of octree in hyperbolic space. So if your evnironment has many obstacles Hyperbolic can be really slow.

I don't know markdown and i'm proud of it.
