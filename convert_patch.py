import base64
import sys
patch = open(sys.argv[1], 'rb').read()
encoded = base64.b85encode(patch)
print(f"TEST_ROM_LEN = {sys.argv[2]}")
print("")
print("FIFO_TESTROM = (")
for i in range(0,len(encoded), 80):
    print ('    b"' + encoded[i:i+80].decode()+'"')
print(")")
