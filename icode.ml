
type classRecord =
  { name: string
  ; super: classRecord
  }

type objectRecord = 
  { t: astType
  ; class: classRecord
  }