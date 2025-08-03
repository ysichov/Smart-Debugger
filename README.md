# SmartDebugger
Smart Debugger - SAP Script program for viewing state - all objects and data structures in debug.

And it can work as debugger time machine which store every variables changes on every steps and show it in convenient format. Usuful to analyse deep structures, several tables simultaneously and search changes in the past!

It is based on ABAP Script technology. So set break-points in any abap code and run. After brake-point is triggered, go to the last tab "script" and paste the script from here - https://github.com/ysichov/Smart-Debugger/blob/master/Z_SDDE.abap

You can save svript and Load every time when needed.

Chhose radio button "Execute directly" and press "Start Script" button.

A window will open, divided into several sections. At the top are the control buttons. The largest window is the source code viewer. On the right are three sections with variables: the main one, where local and global variables are displayed. And two smaller sections with import and export parameters. At the very bottom is a section for displaying the stack/execution history, the history of variable changes.

the script will define all your local and global variables, and show them in a several hierarchy. Also we can switch on additional variable types:
 - button 'SYST' for viewing SYST structure (sy-datum, sy-uname etc variables)
 - button 'CLASS-DATA" will show Static(Global) variables from active classes.
 - button 'LDB' will show all mess of LDB global variables whuch by default is hidden here. In standard debugger we see all the mess and can't avoid it.


Old description in Russian - will be revised soon
- https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
- https://ysychov.wordpress.com/2020/09/08/sdde2/


