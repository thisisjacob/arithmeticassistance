<h1>Mathematical Assistance Project</h1>
  <h2>Project Description</h2>  
  
  A visual program for generating and testing mathematics problems. Includes some arithmetic and geometry problem categories.
  
  <h2>Developers</h2>
  
    thisisjacob - Jacob Holfelder
      Roles:
      - User interface appearance and logic
      - Configuration, game state logic
      - Graphics 
      
    bsb1006 - Bradley Betts
      Roles:
      - Math problem generation and testing

  <h2>Development Information</h2>
  
    - Developed with the DrRacket IDE
    - Racket Version 7.8, Standard Package
    - User interface created with RacketGUI
    - Uses one external library, k-infix

  <h2>Compiler Installation:</h2>
Package Page: https://download.racket-lang.org

1. Download the package with the following settings:
Distribution: Racket
Platform: Any
Variant: Regular

2. Then install the compiler and IDE
    - With Windows, the compiler is installed by running the installation .exe and following the command prompts
    - With Macintosh, the compiler is installed by running the installation .dmg

3. Install required external packages
    - The following external packages must first be installed:
        k-infix
        
    - To install them, open DrRacket, and then go to File >> Install Package... 
    - Type in the name of the packages from the above list one by one
        
        
<h2>Compiling and Running the Program:</h2>

1. Compile the program by compiling the "main.rkt" file found in the program folder

2. Run the program by running the "main.rkt" file.

<h2>Images:</h2>

![image](https://user-images.githubusercontent.com/42303925/98432679-9f994900-208e-11eb-9531-44e963b0ce20.PNG)

![image](https://user-images.githubusercontent.com/42303925/98432693-cce5f700-208e-11eb-9a71-c281c93cd469.PNG)

<h2>Program Structure</h2>

![Presentation graph](https://user-images.githubusercontent.com/42303925/98455949-24469e80-2145-11eb-8d1c-d188d04e4603.png)

<h3>Structure Element Descriptions</h3>

  - <b>mainWindow</b>: Instantiates the other interface windows, creates functions for switching between the windows, passes functions down to mainMenu for use through the other portions of the UI

  - <b>mainMenu</b>: Holds a list of buttons that call the enable function of the difficulty screen, each button is associated with one of the game difficulty constructs under constants. This construct is passed down to the difficulty screen.

  - <b>gradeAndDifficultySelectScreen</b>: Holds a list of buttons for passing the received gamemode, and a selected problem category construct to the problemsScreen

  - <b>drawingInputScreen (or problemsScreen)</b>: Passes the problem category construct to equation generator to generate a problem, which is then drawn onto this screen. Draws mode specific information based upon the received game mode construct. User enters answers into this menu, and the answers correctness is returned to the user.

  - <b>buttonGenerator</b>: Dynamically generates and adds buttons to a given device-contexts, and holds and manages the functions (with passed arguments) that are called by each button.

  - <b>equationGenerator</b>: Determines a problem to generate based on the received problem category. Generates that problem, and calls functions in canvasShapeDrawingFunctions to draw the problems onto the screen. Returns the value of the correct answer for comparing with user submitted answers.

  - <b>canvasShapeDrawingFunctions</b>: Holds multiple functions for drawing problems and information onto the drawingInputScreen. These include: the problem description box, dynamically drawn geometry problems, and the scoreboard if in multiplayer:

  - <b>Constants (folder)</b>: Holds constants and configuration imports for the program as a whole. Examples of these include the game mode and problem category constructs, and the userInterfaceConstants that control aspects of the program's appearance.
  
<h3>Program Structure Issues</h3>

- Roles of each file not as separated as they should be. Improving separation will be of major benefit to the program in the future.

- mainWindow manages too much of the user interface as a whole. Parts of it, such as function and list generation belong in another file entirely, and it would be ideal to change mainWindow into a class for just managing the current visibility of each screen.

- the equationGenerator functions for generating reuse a lot of code that is not encapsulated into functions. This is something that should be improved in order to create a more easily modifiable and readable program.

<h3>General Program Issues</h3>

  - Arithmetic problems currently round intermediate calculations - which can lead to answers that are correct being marked wrong if the user uses a different method of rounding.
  - Geometric problems currently limited to area problems

<h2>Suggestions to Improve the Program</h2>

  - Add more problems, particularly for symbolic algebra.
  - Increase the variety of geometric problems, add three dimensional problems.
  - Migrate to a more advanced user interface library
  - Move to a graphics library that is capable of both 2D and 3D graphics (OpenGL is an available option in Racket, but may be too much for our scale)

