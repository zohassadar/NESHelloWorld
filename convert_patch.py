import base64
patch = open('fifo_testrom.ips', 'rb').read()
encoded = base64.b85encode(patch)
print("fifo_testrom = (")
for i in range(0,len(encoded), 80):
    print ('    b"' + encoded[i:i+80].decode()+'"')
print(")")
