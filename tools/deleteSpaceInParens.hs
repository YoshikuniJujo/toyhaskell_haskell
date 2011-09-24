main = interact deleteSpace

deleteSpace ""				= ""
deleteSpace ('(':' ':cs)		= '(' : deleteSpace cs
deleteSpace (' ':')':cs)		= ')' : deleteSpace cs
deleteSpace ('[':' ':cs)		= '[' : deleteSpace cs
deleteSpace ('\n' : ' ' : ']' : cs)	= '\n' : ' ' : ']' : deleteSpace cs
deleteSpace (' ':']':cs)		= ']' : deleteSpace cs
deleteSpace (c : cs)			= c : deleteSpace cs
