void add(num out, num mod, reg i = null){
    if (i == null){
        int i = 0
    } else {
        ldi #mod.size - 1
    }
    
    // intelligence assisted clear, supporting runtime or compile time
    clc
    
    if typeof(i) == "int" {
        loop {
            lda out[i]
            adc mod[i]
            sta out[i]
            
            if ++i >= mod.size {
                break
            }
        }
    } else {
        loop:
            lda out[i - 1]
            adc mod[i - 1]
            sta out[i - 1]
            
            dei
            bne loop
    }
    
    // i is no longer used after here
    del i
    
    // apply carry
    if mod.size > out.size {
        adc #0
        sta out[mod.size]
    }
    
    // sign flag prompts decrement conditionally
    if mod.endian == 1 {
        lda mod[-1]
        bmi +
            dec out[mod.size]
           
        +:
    }
}

void sub(num out, num mod, reg i = null){
    if (i == null){
        int i = 0
    } else {
        ldi #mod.size - 1
    }
    
    // intelligence assisted set, supporting runtime or compile time
    sec
    
    if typeof(i) == "int" {
        loop {
            lda out[i]
            sbc mod[i]
            sta out[i]
            
            if ++i >= mod.size {
                break
            }
        }
    } else {
        loop:
            lda out[i - 1]
            sbc mod[i - 1]
            sta out[i - 1]
            
            dei
            bne loop
    }
    
    // i is no longer used after here
    del i
    
    // apply carry
    if mod.size > out.size {
        sec #0
        sta out[mod.size]
    }
    
    // sign flag prompts decrement conditionally
    if mod.endian == 1 {
        lda mod[-1]
        bmi +
            inc out[mod.size]
           
        +:
    }
}
/*
    use:
        u8 foo
        
        lda #20
        sta foo
        
        i16 bar
        math.cast(bar, foo)
/*
void cast(num ctx, num new, reg i = null){
    if (i == null){
        // inline interation begins from the back, this is so in the event the user wishes to cast from unsigned
        // to signed they can choose to mask the MSB of the high byte
        // the environment intelligence then will NOT repeat redundant instructions
        // if the intelligence is disabled, there will be a repeat in this task.
        
        int i = new.size - 1
    }
    
    // converting to signed from unsigned requires loss of value with MSB
    int RequiresTrim = i == new.size - 1 && new.signed && !ctx.signed
    
    if (typeof(i) == "int"){
        loop {
            lda ctx[i]
            
            if (RequiresTrim){
                // this loss of value is improper, debug will forbid at runtime
                if (DEBUG){
                    bpl +
                        brk
                    +
                } else {
                // however it is permitted with release
                    and #~$80
                }
            }
            
            sta new[i]
            if (--i < 0) {
                break
            }
        }
        
        // if casting down or to equal size, sign extension is not merited
        if (new.size <= ctx.size){
            return
        }
        
        // sign extend
        sex
        loop {
            sta ctx[i]
        }
    } else {
        if (RequiresTrim){
            lda ctx[ctx.size - 1]
            and #~$80
            sta new[ctx.size - 1]
        }
        
        // safe space/speed efficient redundant iteration prevention
        if (new.size - RequiresTrim == 1){
            lda [ctx.size - 1 - RequiresTrim]
            sta [ctx.size - 1 - RequiresTrim]
            return
        }

        // in the event of requires trim, its handled already
        ldi #ctx.size - RequiresTrim
        int n = (ctx.size / 256) - 1
        loop {
            -:
                lda ctx[i - 1 + (n * 256)]
                sta new[i - 1 + (n * 256)]
                dei
                bne -
                
            if (--n < 0){
                break
            }
        }
        
        // fix uninitialized bytes
        int extend = new.size - ctx.size
        if (!extend){
            return
        } else if (extend == 1){
            sex
            sta new[ctx.size]
            return
        }
        
        // multiple extending uninitialized bytes
        n = ((new.size - ctx.size) / 256) - 1
        sex
        ldi #new.size - ctx.size
        -:
            sta new[i - 1 + (n * 256)]
            dei
            bne -
    }
}