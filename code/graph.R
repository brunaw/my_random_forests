digram <- "
graph LR;
A((Node 1,))-->B((Node 2: X1>50));
A((Node 1))-->C((Node 3: X1<=50));
B-->D((Node 4: X2>0.5 - Red));
B-->E((Node 5: X2<=0.5 - Green));
C-->F((Node 6: X2>0.5 - Green));
C-->G((Node 7: X2<=0.5 - Red));
"
# bolding variables 
digram <- stringr::str_replace_all(
  diagrama, 
  pattern = "([XDU][0-9])", 
  replacement = "<b>\\1</b>")

DiagrammeR::DiagrammeR(digram)

