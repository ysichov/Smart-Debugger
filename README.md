# SmartDebugger
Smart Debugger - SAP Script program for viewing state - all objects and data structures in debug.

And it can work as debugger time machine which store every variables changes on every steps and show it in convenient format. Usuful to analyse deep structures, several tables simultaneously and search changes in the past!

It is based on ABAP Script technology. So set break-points in any abap code and run. After break-point is triggered, go to the last tab "script" and paste the script from here - https://github.com/ysichov/Smart-Debugger/blob/master/Z_SDDE.abap

You can save script and Load every time when needed.

Chhose radio button "Execute directly" and press "Start Script" button.

<img width="1526" height="823" alt="image" src="https://github.com/user-attachments/assets/54e99fad-be8c-4c6b-aad9-989ab141fbe9" />


A window will open, divided into several sections. At the top are the control buttons. The largest window is the source code viewer. On the right are three sections with variables: the main one, where local and global variables are displayed. And two smaller sections with import and export parameters. At the very bottom is a section for displaying the stack/execution history, the history of variable changes.

the script will define all your local and global variables, and show them in a several hierarchy. Also we can switch on additional variable types:
 - button 'SYST' for viewing SYST structure (sy-datum, sy-uname etc variables)
 - button 'CLASS-DATA' will show Static(Global) variables from active classes.
 - button 'LDB' will show all mess of LDB global variables whuch by default is hidden here. In standard debugger we see all the mess and can't avoid it.

The Next Big Thing - is hierarchiсal trees for structures, deep structures and class instance objects. We have a 'helicopter view' for all variables at any step!

Tables wih header lines and defined with 'OCCURS' are shown as variable_name[] next to its header structure variable.

Double-click on any table variable will show it in separate window! And we don't have no limitations for windows quantity. If you have a big display you can open more then 10 tables simultaneously and analyse it!

Sometimes we don't want to see empty or Initial variables. Just press button 'Initials" and all empty variables will disappear. Button works as On/Of switch.

That was all about static. But this debugger plugin can execute program run, save all steps and all variables changes. And we can analyse this set of date in 2 directions: to the Future (by default - Button 'Forward') or to the past (Button 'Backward').

So for direction 'Forward' we have the same 4 debug button as in classic debugger:
 - Step into (F5) - one step ahead
 - Step over (F6) - one operation ahead.
 - Step out (F7) - exit from current stack kevel
 - Continue (F8) - to the first break or watch point in the future

 For 'Backward' we have a smaller set of buttons:
 - Step back  - one step back
 - to the previous stop condition - to the first break or watch point in the past

 It is very easy to create watchpointes. Just double click on any variable. It will be added to the watchpoint table. To delete watchpoint - again press double click on the variable. With button 'Clear vars' we can  delete all watchpoints.

 By default we have first button state 'Visualisation ON' - with this option we can see live program run for every step!!!
 
 Also by default (button 'History On' we are saving all variables changes)
 
 Button 'Depth' have Depth 9 - which means that we analyse 9 stack levels. We can set it from 0 to 9. Zero means that we are writing history only for current stack level. Depth 1 - means that we are saving current level and level of functions/method/forms which run on current level... And so on

 Button 'Only Z' means that we are saving history by default only for Z code. We can switch it to 'Z & Standard' state and save changes in Standard code also.
 
Old description in Russian - will be revised soon
- https://ysychov.wordpress.com/2020/07/27/abap-simple-debugger-data-explorer/
- https://ysychov.wordpress.com/2020/09/08/sdde2/


