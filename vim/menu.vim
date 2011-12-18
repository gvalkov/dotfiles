" destroy existing popup menu
silent! unmenu PopUp
silent! unmenu! PopUp

an <silent> 1.10 PopUp.&Undo    u             
an <silent> 1.15 PopUp.-SEP1-   <Nop>

vmenu 1.20 PopUp.Cu&t		"+x
vmenu 1.30 PopUp.&Copy		"+y
cmenu 1.30 PopUp.&Copy		<C-Y>
nmenu 1.40 PopUp.&Paste		"+gP
cmenu 1.40 PopUp.&Paste		<C-R>+
