# SmartDebugger

<img width="400" height="390" alt="image" src="https://github.com/user-attachments/assets/4cd84ec5-2ff1-482a-964a-6a6af14511f3" />


Smart Debugger - SAP Script program for viewing state - all objects and data structures in debug.

#  [ABAP Mermaid](https://github.com/WegnerDan/abapMermaid) should be installed to  draw diagrams!!!

And it can work as debugger time machine which store every variables changes on every steps and show it in convenient format. Usuful to analyse deep structures, several tables simultaneously and search changes in the past!

It is based on ABAP Script technology. So set break-points in any abap code and run. After break-point is triggered, go to the last tab "script" and paste the script from [paste the script from here](https://github.com/ysichov/Smart-Debugger/blob/master/src/z_smart_debugger.prog.abap). 

You can save script and Load every time when needed.

Choose radio button "Execute directly" and press "Start Script" button.

<img width="1526" height="823" alt="image" src="https://github.com/user-attachments/assets/54e99fad-be8c-4c6b-aad9-989ab141fbe9" />



A window will open, divided into several sections. At the top are the control buttons. The largest window is the source code viewer. On the right are three sections with variables: the main one, where local and global variables are displayed. And two smaller sections with import and export parameters. At the very bottom is a section for displaying the stack/execution history, the history of variable changes.
<img width="1749" height="900" alt="image" src="https://github.com/user-attachments/assets/2d6c79e4-9473-4d2c-a290-85f7bc5cfeb3" />


the script will define all your local and global variables, and show them in a several hierarchy. Also we can switch on additional variable types:
 - button 'SYST' for viewing SYST structure (sy-datum, sy-uname etc variables)
 - button 'CLASS-DATA' will show Static(Global) variables from active classes.
 - button 'LDB' will show all mess of LDB global variables whuch by default is hidden here. In standard debugger we see all the mess and can't avoid it.

The Next Big Thing - is a hierarchiсal trees for structures, deep structures and class instance objects. We have a 'helicopter view' for all variables at any step!

Tables wih header lines and defined with 'OCCURS' are shown as variable_name[] next to its header structure variable.
Double-click on any table variable will show it in separate window! And we don't have no limitations for windows quantity. If you have a big display you can open more then 10 tables simultaneously and analyse it!

<img width="1686" height="654" alt="image" src="https://github.com/user-attachments/assets/0f3bebe8-14cc-42ac-9c21-751ce5cbfcfd" />


Sometimes we don't want to see empty or Initial variables. Just press button 'Initials" and all empty variables will disappear. Button works as On/Of switch.

Also I think it is a good idea to seperate importing and returning/exporting parameters into different windows.
<img width="1748" height="827" alt="image" src="https://github.com/user-attachments/assets/1263a948-46a6-4017-a898-e61f94d79cc0" />


That was all about static. But this debugger plugin can execute program run, save all steps and all variables changes. And we can analyse this set of date in 2 directions: to the Future (by default - Button 'Forward') or to the past (Button 'Backward').

<img width="1025" height="40" alt="image" src="https://github.com/user-attachments/assets/f6e2cce9-125d-43ed-8690-fd2cb2faf3b5" />


So for direction 'Forward' we have the same 4 debug button as in classic debugger:
 - Step into (F5) - one step ahead
 - Step over (F6) - one operation ahead.
 - Step out (F7) - exit from current stack kevel
 - Continue (F8) - to the first break or watch point in the future

 For 'Backward' we have a smaller set of buttons:
 - Step back  - one step back
 - to the previous stop condition - to the first break or watch point in the past

<img width="1214" height="43" alt="image" src="https://github.com/user-attachments/assets/ec84be4b-fdf6-4000-a199-37c8b3ce300f" />



 It is very easy to create watchpointes. Just double click on any variable. It will be added to the watchpoint table. To delete watchpoint - again press double click on the variable. With button 'Clear vars' we can  delete all watchpoints.

 <img width="906" height="438" alt="image" src="https://github.com/user-attachments/assets/7731bab7-49b2-4133-b3b5-63cca1561d4b" />
Buttons were refactored.

 Button - Alpha - fast version which analyse ABAP code to reduce checked variables. It is working like switch to Beta - slower but more precise version.
 Button 'History On' we are saving all variables changes - by default
 
 Button 'Depth' have Depth 9 - which means that we analyse 9 stack levels. We can set it from 0 to 9. Zero means that we are writing history only for current stack level. Depth 1 - means that we are saving current level and level of functions/method/forms which run on current level... And so on

 Button 'Only Z' means that we are saving history by default only for Z code. We can switch it to 'Z & Standard' state and save changes in Standard code also.

 Button "Diagram" - will show all calls which were performed along code execution as Mermaid Diagram. This button will be active only when Mermaid is installed
https://github.com/ysichov/abapMermaid - should be used this fork with scroll enabled
<img width="1855" height="851" alt="image" src="https://github.com/user-attachments/assets/e2e3b4a7-988f-4e66-8068-65383f63aed4" />

 
Finally **Smart debugger** is becoming really Smart and can save time or money as Time is Money )

After double-click on any variable(1) it works as watchpoint and collects not only selected variables but dependents(4) also (forms/methods/functions call). Button "Show the origin"(2) will paint all steps in the code where variables were changed and paints Mermaid diagram for it. Just Alpha version... And I disappointed with AI. It couldn't give me such results as good algorythm which can interprete ABAP code.

<img width="1676" height="904" alt="image" src="https://github.com/user-attachments/assets/e19e609a-dfe2-402b-beaf-4b83e86d9eb7" />

Button "Coverage" - will show code coverage on all stacks levels

<img width="1752" height="894" alt="image" src="https://github.com/user-attachments/assets/2490c4f5-f24e-47e5-adcd-ea5558b62372" />

Iterators example - http://zevolving.com/2012/01/iterator-design-pattern-to-access-linked-list/
<img width="1743" height="881" alt="image" src="https://github.com/user-attachments/assets/02a0666b-25bd-4904-ad2b-b648d0f08daf" />
