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

mainWindow: Instantiates the other interface windows, creates functions for switching between the windows, passes functions down to mainMenu for use through the other portions of the UI

mainMenu: Holds a list of buttons that call the enable function of the difficulty screen, each button is associated with one of the game difficulty constructs under constants. This construct is passed down to the difficulty screen.

gradeAndDifficultySelectScreen: Holds a list of buttons for passing the received gamemode, and a selected problem category construct to the problemsScreen

drawingInputScreen (or problemsScreen): Passes the problem category construct to equation generator to generate a problem, which is then drawn onto this screen. Draws mode specific information based upon the received game mode construct. User enters answers into this menu, and the answers correctness is returned to the user.
