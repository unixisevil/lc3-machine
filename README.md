### lc3-machine

port [lc3-vm](https://github.com/justinmeiners/lc3-vm) to [rust](https://github.com/rust-lang/rust). 

####  material about  lc3 

* [lc3 isa spec](https://www.cs.colostate.edu/~cs270/.Fall18/resources/PattPatelAppA.pdf)

* [lc3 toolchain download](https://highered.mheducation.com/sites/0072467509/student_view0/lc-3_simulator.html)

* [lc3 textbook  download](https://drive.google.com/file/d/0B_y5VoosmiugNWpjM1VJTlM1RGM/view)

#### usage
download lc3 toolchain, use lc3as generate object code from lc3 asm file.
get [2048.asm](https://github.com/rpendleton/lc3-2048/blob/master/2048.asm) ,and then

```bash
lc3as  2048.asm 
```
then:

```bash
./l3-machine  2048.obj 
```
