# copied from https://github.com/aspittel/advent-of-code/ to generate debug output in order to find my evil bug
from itertools import permutations

def get_permutations(li):
    for permutation in permutations(li):
        yield permutation

def get_modes(modes):
    return [int(mode) for mode in [modes[2], modes[1], modes[0], modes[3:]]]


class Computer:
    def __init__(self, data, amp_idx):
        self.idx = 0
        self.amp_idx = amp_idx
        self.data = data[:]
        self.done = False
        self.output = None
        self.inputs = []

    def get_params(self, mode1, mode2):
        return self.get_param(mode1, 1), self.get_param(mode2, 2)

    def get_param(self, mode, increment):
        if mode == 0:
            return self.data[self.data[self.idx + increment]]
        return self.data[self.idx + increment]

    def add(self, param1, param2):
        return param1 + param2

    def multiply(self, param1, param2):
        return param1 * param2

    def less_than(self, param1, param2):
        return 1 if param1 < param2 else 0

    def equals(self, param1, param2):
        return 1 if param1 == param2 else 0

    def jump_if_true(self, mode1, mode2):
        param1, param2 = self.get_params(mode1, mode2)
        return param2 if param1 != 0 else self.idx + 3

    def jump_if_false(self, mode1, mode2):
        param1, param2 = self.get_params(mode1, mode2)
        return param2 if param1 == 0 else self.idx + 3

    def calculate(self, input_val):
        self.inputs.append(input_val)
        while True:
            mode1, mode2, mode3, opcode = get_modes(f"{self.data[self.idx]:05}")
            if opcode == 1:
                param1, param2 = self.get_params(mode1, mode2)
                self.data[self.data[self.idx + 3]] = self.add(param1, param2)
                self.idx += 4
            elif opcode == 2:
                param1, param2 = self.get_params(mode1, mode2)
                self.data[self.data[self.idx + 3]] = self.multiply(param1, param2)
                self.idx += 4
            elif opcode == 3:
                self.data[self.data[self.idx + 1]] = self.inputs.pop(0)
                self.idx += 2
            elif opcode == 4:
                self.output = self.data[self.data[self.idx + 1]]
                self.idx += 2
                print(f"amp #{self.amp_idx} - Opcode: {opcode}; next idx: {self.idx}; outputting {self.output}")
                return self.output
            elif opcode == 5:
                self.idx = self.jump_if_true(mode1, mode2)
            elif opcode == 6:
                self.idx = self.jump_if_false(mode1, mode2)
            elif opcode == 7:
                param1, param2 = self.get_params(mode1, mode2)
                self.data[self.data[self.idx + 3]] = self.less_than(param1, param2)
                self.idx += 4
            elif opcode == 8:
                param1, param2 = self.get_params(mode1, mode2)
                self.data[self.data[self.idx + 3]] = self.equals(param1, param2)
                self.idx += 4
            elif opcode == 99:
                self.done = True
                print(f"amp #{self.amp_idx} - idx: {self.idx}; Opcode: {opcode}; Done. Outputting {self.output}")
                return self.output


# with open("data/day7.txt") as _file:
#     for line in _file:
#         input_vals = [int(num) for num in line.split(",")]
#         max_output_signal = 0
#         for permutation in get_permutations([0, 1, 2, 3, 4]):
#             output_signal = 0
#             for input_signal in permutation:
#                 computer = Computer(input_vals[:])
#                 computer.inputs.append(input_signal)
#                 output_signal = computer.calculate(output_signal)
#             max_output_signal = max(max_output_signal, output_signal)
#         print(f"Part 1: {max_output_signal}")
#
#         max_output_signal_2 = 0
#         best_permutation = []
#         for permutation in get_permutations([5, 6, 7, 8, 9]):
#             computers = [Computer(input_vals[:]) for _ in range(5)]
#             output_signal = 0
#             for computer, phase_setting in zip(computers, permutation):
#                 computer.inputs.append(phase_setting)
#             while computers[-1].done == False:
#                 for computer in computers:
#                     output_signal = computer.calculate(output_signal)
#             if(output_signal > max_output_signal_2):
#                 max_output_signal_2 = output_signal
#                 best_permutation = permutation
#         print(f"Part 2: {max_output_signal_2}. Permutation was: {best_permutation}")

with open("data/day7.txt") as _file:
    for line in _file:
        input_vals = [int(num) for num in line.split(",")]

        max_output_signal_2 = 0
        best_permutation = []
        permutation = [8, 9, 7, 6, 5]
        computers = [Computer(input_vals[:], amp_idx) for amp_idx in range(5)]
        output_signal = 0
        for computer, phase_setting in zip(computers, permutation):
            computer.inputs.append(phase_setting)
        while computers[-1].done == False:
            for computer in computers:
                output_signal = computer.calculate(output_signal)
        print(f"Part 2 - debugging: {output_signal}")
