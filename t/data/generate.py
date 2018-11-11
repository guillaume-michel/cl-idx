import idx2numpy
import numpy as np

def generate_idx_files(datas, types):
    for type_str in types:
        type = getattr(np, type_str)
        for i, data in enumerate(datas):
            a = np.array(data, dtype=type)
            name = type_str + '_' + str(i).zfill(2) + '.idx'
            idx2numpy.convert_to_file(name, a)


unsigned_types = ['uint8']

signed_types = ['int8', 'int16', 'int32', 'float32', 'float64']

unsigned_datas = [
    [0,1,2,3],
    [[0,1,2,3], [4,5,6,7]],
    [[[0,1,2,3], [4,5,6,7]], [[8,9,10,11], [12,13,14,15]]],
]

signed_datas = [
    [0,1,-2,3],
    [[0,1,-2,3], [4,-5,6,7]],
    [[[0,1,-2,3], [4,-5,6,7]], [[8,9,10,11], [12,13,14,15]]],
]

generate_idx_files(unsigned_datas, unsigned_types)
generate_idx_files(signed_datas, signed_types)
