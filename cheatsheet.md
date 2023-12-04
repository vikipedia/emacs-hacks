# Cheatsheet

### selection
* C-x h - select whole file

### undo/redo
* C-/ - undo
* C-g C-/ - redo

### yank/paste

* C-y - yank
* M-y - cycle among previous yanks

### moving
go to top of file M-<
go to end of file M->
come back to position from where we moved to end or start of buffer C-u C-SPC

### case switching
 convert word to lower case M-l
 convert word to upper case M-u
 capitalize following word M-c
 
 
### YAsnippets
insert predefined snippet C-c-&-C-s 

### inserting enter in search/replace regex
C-q C-j


### inserting output of shell command
C-u M-! cmd
for example to insert current date and time 
C-u M-! date

### Switch buffer
C-x [left]  move previous buffer to current space
C-x [right] move next buffer to current space

### increase/decrease font size
C-x C-+ to increase
C-x C-+ to decrease
after one stroke of increase or decrese immediately just +/- can be used to alter size of font

### goto line
M-g M-g

### search word under cursor
M-b C-s C-w -> M-b will move cursor to word start, C-s will start search, C-w will add current word to search
