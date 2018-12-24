#[macro_use]
extern crate num_derive;

use { 
    num, 
    libc, 
    std::{
        io::{self, Read, Write},
        env, ptr, mem, fs::File,
        process,
    },
    termios::*
};

const MEM_SIZE: usize = 65536;

enum Reg
{
    R0, 
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    Pc,    //program counter
    Cond,
    Count
}

#[derive(FromPrimitive, Debug)]
enum OpCode
{
    Br,     //branch  0x00
    Add,    //add     0x01
    Ld,     //load    0x02
    St,     //store   0x03
    Jsr,    //jump register 0x04
    And,    //bitwise and   0x05
    Ldr,    //load register  0x06
    Str,    //store register 0x07
    Rti,    //unused        0x08
    Not,    //bitwise not   0x09
    Ldi,    //load indirect 0x0a
    Sti,    //store indirect 0x0b
    Jmp,    //jump          0x0c
    Res,    //reserved (unused) 0x0d
    Lea,    //load effective address 0x0e
    Trap    // execute trap 0x0f
}

enum Flag
{
    Pos = 1 << 0,  //postive
    Zro = 1 << 1,  //zero
    Neg = 1 << 2,  //negtive
}

enum MmReg
{
    Kbsr = 0xfe00, //keyboard status register
    Kbdr = 0xfe02  //keyboard data register
}

//Trap Codes
#[derive(FromPrimitive, Debug)]
enum TrapCode
{
    Getc = 0x20,  // get character from keyboard
    Out = 0x21,   // output a character 
    Puts = 0x22,  // output a word string 
    In = 0x23,    // input a string 
    Putsp = 0x24, // output a byte string
    Halt = 0x25   // halt the program
}

struct Lc3 {
    reg :[u16; Reg::Count as usize],
    mem :[u16; 65536],
}

fn getchar() ->  Option<io::Result<u8>> {
   io::stdin().bytes().take(1).next()
}

fn putchar(c: u8) -> bool {
    let buf = [c;1];
    match io::stdout().write(&buf).iter().next() {
        Some(_) => true,
        None   =>  false,
    } 
}

fn stdin_buffering(enable : bool) -> io::Result<()> {
  let mut termios = Termios::from_fd(0)?;
  match enable {
      true => termios.c_lflag |= ICANON| ECHO,
      false => termios.c_lflag &= !(ICANON|ECHO),
  }
  tcsetattr(0, TCSANOW, &termios)?;
  Ok(())
}


fn key_activate() -> bool {
    unsafe { 
        let mut rset: libc::fd_set = mem::uninitialized();
        libc::FD_ZERO(&mut rset);
        libc::FD_SET(0, &mut rset);
        let mut timeout  = libc::timeval{ tv_sec: 0, tv_usec:0 };
        let wset : *mut libc::fd_set = ptr::null_mut();
        let eset : *mut libc::fd_set = ptr::null_mut();
        libc::select(1, &mut rset, wset, eset, &mut timeout) == 1 
    }
}

fn sign_extend(mut x :u16, bits: i32) -> u16 {
    match (x >> (bits-1)) & 0x1 {
        1 =>  {x |= 0xffff << bits; x },
        _ =>  x,
    }
}

impl Lc3 {
    fn update_flags(&mut self, r :u16) {
        match self.reg[r as usize] {
            0 =>  self.reg[Reg::Cond as usize] = Flag::Zro as u16,
            x  if x >> 15  == 1 => self.reg[Reg::Cond as usize] = Flag::Neg as u16,
            _ =>  self.reg[Reg::Cond as usize] = Flag::Pos as u16,
        }
    }

    fn read_mem(&mut self, addr: u16) -> u16 {
        let kbsr  = MmReg::Kbsr as usize;
        let kbdr  = MmReg::Kbdr as usize;
        let addr = addr as usize;
        if addr == kbsr {
            match key_activate(){
                true => {
                    self.mem[kbsr] =  1 << 15;
                    self.mem[kbdr] = getchar().unwrap().unwrap() as u16;
                },
                false =>  {
                    self.mem[kbsr] = 0;
                }
            }
        }
        self.mem[addr]
    }

    fn write_mem(&mut self, addr: u16, val :u16) {
        self.mem[addr as usize] = val;
    }

    fn add_pc(&mut self, off :u16) {
         self.reg[Reg::Pc as usize] = self.reg[Reg::Pc as usize].wrapping_add(off);
    }

    fn jump_to(&mut self, addr: u16){
        self.reg[Reg::Pc as usize] = addr;
    }

    fn execute(&mut self) {
        self.reg[Reg::Pc as usize] = 0x3000;
        loop {
            let instr = self.read_mem(self.reg[Reg::Pc as usize]);
            let op = num::FromPrimitive::from_u16(instr >> 12).unwrap();
            //println!("got op : {:?}", op);
            self.add_pc(1);
            match op {
               OpCode::Add => {
                   let dr = (instr >> 9) & 0x7;
                   let sr1 = (instr >> 6) & 0x7;
                   let imm_mode = (instr >> 5) & 0x1;
                   match imm_mode {
                       1  => {
                           let imm5 = instr & 0x1f;
                           self.reg[dr as usize] = self.reg[sr1 as usize].wrapping_add(sign_extend(imm5, 5));
                       },
                       _  => {
                           let sr2 = instr & 0x7;
                           self.reg[dr as usize] = self.reg[sr1 as usize].wrapping_add(self.reg[sr2 as usize]);
                       },
                   }
                   self.update_flags(dr);
               },
               OpCode::And =>  {
                   let dr = (instr >> 9) & 0x7;
                   let sr1 = (instr >> 6) & 0x7;
                   let imm_mode = (instr >> 5) & 0x1;
                   match imm_mode {
                       1  => {
                           let imm5 = instr & 0x1f;
                           self.reg[dr as usize] = self.reg[sr1 as usize] & sign_extend(imm5, 5);
                       },
                       _  => {
                           let sr2 = instr & 0x7;
                           self.reg[dr as usize] = self.reg[sr1 as usize] & self.reg[sr2 as usize];
                       },
                   }
                   self.update_flags(dr);
               },
               OpCode::Not =>  {
                   let dr = (instr >> 9) & 0x7;
                   let sr = (instr >> 6) & 0x7;
                   self.reg[dr as usize] = !self.reg[sr as usize];
                   self.update_flags(dr);
               },
               OpCode::Br  => {
                   let off9 = sign_extend(instr & 0x1ff, 9);
                   let cond_flag = (instr >> 9) & 0x7;
                   if cond_flag & self.reg[Reg::Cond as usize] != 0 {
                       self.add_pc(off9);
                   }
               },
               OpCode::Jmp => {
                   let  base_r = (instr >> 6 ) & 0x7;
                   self.jump_to(self.reg[base_r as usize]);
               },
               OpCode::Jsr => {
                   self.reg[Reg::R7 as usize] = self.reg[Reg::Pc as usize];
                   let long_mode = (instr>>11) & 0x1;
                   match long_mode == 1 {
                       true =>  { //jsr
                           let off11 = sign_extend(instr&0x7ff, 11);
                           self.add_pc(off11);         
                       },
                       false => {//jsrr
                           let  base_r = (instr >> 6 ) & 0x7;
                           self.jump_to(self.reg[base_r as usize]); 
                       }
                   }
               },
               OpCode::Ld => {
                   let  dr = (instr >> 9 ) & 0x7;
                   let off9 = sign_extend(instr & 0x1ff, 9);
                   let addr = self.reg[Reg::Pc as usize].wrapping_add(off9);
                   self.reg[dr as usize] = self.read_mem(addr);
                   self.update_flags(dr);
               },
               OpCode::Ldi => {
                   let  dr = (instr >> 9 ) & 0x7;
                   let off9 = sign_extend(instr & 0x1ff, 9);
                   let addr = self.reg[Reg::Pc as usize].wrapping_add(off9);
                   let addr = self.read_mem(addr);
                   self.reg[dr as usize] = self.read_mem(addr);
                   self.update_flags(dr);
               },
               OpCode::Ldr => {
                   let  dr = (instr >> 9 ) & 0x7;    
                   let  base_r = (instr >> 6 ) & 0x7; 
                   let  off6 = sign_extend(instr & 0x3f, 6);
                   let  addr = self.reg[base_r as usize].wrapping_add(off6);
                   self.reg[dr as usize] = self.read_mem(addr);
                   self.update_flags(dr);
               },
               OpCode::Lea => {
                   let  dr = (instr >> 9 ) & 0x7;
                   let off9 = sign_extend(instr & 0x1ff, 9);
                   self.reg[dr as usize] = self.reg[Reg::Pc as usize].wrapping_add(off9);
                   self.update_flags(dr);
               },
               OpCode::St => {
                   let  sr = (instr >> 9 ) & 0x7;
                   let off9 = sign_extend(instr&0x1ff, 9);
                   self.write_mem(self.reg[Reg::Pc as usize].wrapping_add(off9), self.reg[sr as usize]);
               },
               OpCode::Sti => {
                   let  sr = (instr >> 9 ) & 0x7;
                   let off9 = sign_extend(instr&0x1ff, 9);
                   let addr = self.read_mem(self.reg[Reg::Pc as usize].wrapping_add(off9));
                   self.write_mem(addr , self.reg[sr as usize]);
               },
               OpCode::Str => {
                   let  sr = (instr >> 9 ) & 0x7;  
                   let  base_r = (instr >> 6 ) & 0x7;
                   let  off6 = sign_extend(instr & 0x3f, 6);
                   self.write_mem(self.reg[base_r as usize].wrapping_add(off6)  , self.reg[sr as usize]);
               },
               OpCode::Trap=> {
                   let trap_code = num::FromPrimitive::from_u16(instr&0xff).unwrap();
                   match trap_code {
                       TrapCode::Getc => {
                           self.reg[Reg::R0 as usize] = getchar().unwrap().unwrap() as u16;
                       },
                       TrapCode::Out => {
                           putchar(self.reg[Reg::R0 as usize] as u8);
                           io::stdout().flush();
                       },
                       TrapCode::Puts => {
                           let index = self.reg[Reg::R0 as usize] as usize;
                           for c  in  &self.mem[index..] {
                               if *c != 0 {
                                   putchar(*c as u8);
                               }else{
                                   break
                               }
                           }
                           io::stdout().flush();
                       },
                       TrapCode::In => {
                           print!("enter a character:");
                           self.reg[Reg::R0 as usize] = getchar().unwrap().unwrap() as u16;
                       },
                       TrapCode::Putsp => {
                           let index = self.reg[Reg::R0 as usize] as usize;
                           for c  in  &self.mem[index..] {
                               if *c != 0 {
                                   putchar((*c & 0xff ) as u8);
                                   if *c >> 8 != 0 {
                                       putchar((*c >> 8) as u8);
                                   }
                               }else{
                                   break
                               }
                           }
                           io::stdout().flush();
                       },
                       TrapCode::Halt => {
                           println!("Halt");
                           break
                       },
                   }
               },
               OpCode::Res | OpCode::Rti | _ =>  panic!("invalid instruction"),
            }
        }
    }

    fn load_exe_image_file(&mut self, path: &str) -> io::Result<()> {
        let mut file  = File::open(path)?;
        let mut orig = [0u8;2];

        file.read_exact(&mut orig)?;
        let orig =  unsafe { mem::transmute::<_, u16>(orig).to_be() };
        let mut mem = unsafe { 
            mem::transmute::<_, [u8;MEM_SIZE*2]>(self.mem)
        };

        let size =  file.read(&mut mem[orig as usize*2 ..])?;
        let orig =  orig as usize;
        self.mem = unsafe {
            mem::transmute::<_, [u16;MEM_SIZE]>(mem)
        };
        for iw  in &mut self.mem[orig .. orig + size/2] {
            *iw = iw.to_be();
        }
        //self.dump_mem(orig, size/2);
        Ok(())
    }

    fn dump_mem(&self, s: usize , len: usize) {
        for e  in  &self.mem[s .. s+len] {
            print!("{:04x} ", *e); 
        }
        println!();
    }
}


fn main() -> io::Result<()>  {
    let args: Vec<_> = env::args().collect();

    if args.len() < 2 {
        eprintln!("lc3 obj-file ...");
        process::exit(1);
    }

    let mut lc3: Lc3 = Lc3{
        reg: [0u16; Reg::Count as usize],
        mem: [0u16; MEM_SIZE],
    };

    for f in &args[1..] {
            if lc3.load_exe_image_file(&f).is_err() {
                eprintln!("failed load image file {}", f);
                process::exit(1);
            }
    }

    stdin_buffering(false)?;
    lc3.execute();
    stdin_buffering(true)
}
