///This dictates the ABI rule to follow
pub enum ABIClass {
    INTEGER, //Goes to the general purpose regster
    SSE,     //Goes to XMM register(works for floats)
    MEMORY,  //Too big and unaligned this gets passed on the stack
    SRET,    //Return via a hudden struct pointer
}
