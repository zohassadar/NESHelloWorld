import base64
import sys
patch = open(sys.argv[1], 'rb').read()
encoded = base64.b85encode(patch)
print(f"TEST_ROM_LEN = {sys.argv[2]}")
print(f'TEST_ROM_SHA1SUM = "{sys.argv[3]}"')
print("")
print("fifo_testrom = (")
for i in range(0,len(encoded), 80):
    print ('    b"' + encoded[i:i+80].decode()+'"')
print(")")
