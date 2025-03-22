import Lean.Data.HashMap

namespace AmadeusKurisu

structure Socket where
  id : Nat
  deriving Inhabited

def Socket.mk (_family _type : Nat) : IO Socket :=
  IO.println "Socket API not available in this Lean version" *>
  pure { id := 0 }

def Socket.setOption (_s : Socket) (_opt _val : Nat) : IO Bool :=
  pure true

def Socket.bind (_s : Socket) (_addr : Nat) : IO Unit :=
  pure ()

def Socket.listen (_s : Socket) (_maxConn : Nat) : IO Unit :=
  pure ()

def Socket.accept (_s : Socket) : IO (Socket × Nat) :=
  pure ({ id := 0 }, 0)

def Socket.recv (_s : Socket) (_buf : ByteArray) : IO Nat :=
  pure 0

def Socket.send (_s : Socket) (_data : ByteArray) : IO Nat :=
  pure 0

def Socket.close (_s : Socket) : IO Unit :=
  pure ()

end AmadeusKurisu
