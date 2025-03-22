import Amadeus

def main : IO Unit :=
  AmadeusKurisu.main "test-key" "https://api.example.com/v1/chat/completions" 3000
