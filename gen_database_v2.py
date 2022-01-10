import subprocess
import random
import os
import statistics
import json


CPU_MAX_TIME = 150


def gen_input_data(number_of_exams, prob_collision, seed, output_name):
    """
    This function will call gen.py and will generate a new data file with the parameters of number_of_exams,
    prob_collision, seed, and output_name.

    :param number_of_exams: int number of exams
    :param prob_collision: float probability of collision
    :param seed: int random number generator seed
    :param output_name: string name of the ouput file
    :return:
    """

    if prob_collision > 1.0 or prob_collision < 0:
        raise ValueError("Probability of collision must be inferior to 1")

    if number_of_exams < 0:
        raise ValueError("Number of exams must be a positive integer")

    if seed < 0:
        raise ValueError("Seed number must be a positive integer")

    os.system(f"python gen.py {number_of_exams} {prob_collision} {seed} {output_name}.txt")


def run_code(code_name, seed, cpu_max_time, input_file):
    """
    This function will call code1.exe or code2.exe, depending on the code_name parameter. Code1 or Code2 require
    3 parameters that are given by seed, cpu_max_time, and input_file.
    The function will return a tuple with the minimal number of slots and the cpu time.

    :param code_name: string referring to the the scheduling programing being used.
    :param seed: int random number generator seed
    :param cpu_max_time: int maximum time allow for usage of cpu.
    :param input_file: string name of the input data file.
    :return: tuple(number of slots, cpu time)
    """
    proc = subprocess.run(f"{code_name}.exe {seed} {cpu_max_time} {input_file}.txt",
                          shell=True, stdout=subprocess.PIPE, text=True)
    output = proc.stdout.replace(" \n", "")
    number_of_slots, cpu_time = output.split(" ")
    return int(number_of_slots), float(cpu_time)


def get_number_of_collisions(file_name):
    """
    This function opens the input data file generated from gen.py and extracts the total number of collisions.

    :param file_name: string path to the input data file
    :return: int total number of collisisons.
    """
    with open(f"{file_name}.txt") as file:
        data = file.readline()
        _, collisions, _ = data.split(" ")

    return collisions


def save_data(data, name, new_file):
    """
    This function will save the data.

    :param dataset: list containing the dataset.
    :param name: name of the output file
    :return:
    """
    with open(f"{name}.txt", 'a') as file:
        if new_file:
            file.write("File_ID Code_Type Number_of_Exams Collision_Prob Collisions Number_Slots CPU_Time \n")
            file.write(f"{' '.join([str(output) for output in data])} \n")
        else:
            file.write(f"{' '.join([str(output) for output in data])} \n")


def remove_file(input_file, linux=False):
    """
    This function removes the temporary data input file generated from gen.py, after its usage in both code1 and code2.

    :param input_file: path to the data file to be removed
    :param linux: bool flag to physically express the operating system.
    :return:
    """
    if linux:
        os.system(f"rm {input_file}.txt")
    else:
        os.system(f"Del {input_file}.txt")


def save_seeds(seeds, file_name):
    """
    This function saves the seeds used for code1 and code2 in a json file name seeds.txt.

    :param seeds: list of seeds.
    :param file_name: string file name
    :return:
    """
    with open(f"{file_name}.txt", 'a') as file:
        json.dump(seeds, file)


def initialize_seeds(n, file_name):
    """
    This function will load, if available, a set of previously used seeds. If not available the function will generate
    a new set of n seeds.

    :param n: int number of desired seeds to be used.
    :param file_name: string file name
    :return:
    """
    try:
        with open(f"{file_name}.txt") as file:
            seeds = json.load(file)
        flag = False
    except OSError as error:
        print("No previous seeds. Initializing a new set of seeds")
        seeds = random.sample(range(1000), n)
        flag = True

    return seeds, flag


def main():
    exams = [i for i in range(40, 105, 5)]
    probabilities = [0.10, 0.20, 0.30, 0.40, 0.50, 1.00]
    gen_data_seeds, gen_seed_flag = initialize_seeds(10, "gen_seeds")
    code_seeds, code_seed_flag = initialize_seeds(10, "code_seeds")

    if gen_seed_flag:
        save_seeds(gen_data_seeds, "gen_seeds")

    if code_seed_flag:
        save_seeds(code_seeds, "code_seeds")

    file_id = 1
    file_name = "input_data"
    codes = ["code1", "code2"]
    #output_data = []
    new_file = True
    print("<-----Starting----->", end="\n\n")
    for exam in exams:
        for prob in probabilities:
            for gen_seed in gen_data_seeds:
                gen_input_data(exam, prob, gen_seed, file_name)
                collisions = get_number_of_collisions(file_name)
                for code in codes:
                    for code_seed in code_seeds:
                        print(f"Running code = {code} with: Exam_number = {exam}, Probability = {prob}", end="\n")
                        number_slots, cpu_time = run_code(code, code_seed, CPU_MAX_TIME, file_name)
                        print(f"CPU time {cpu_time}", end="\n\n")
                        new_data = (file_id, code, exam, prob, collisions, number_slots, cpu_time)
                        save_data(new_data, "output", new_file)
                        #output_data.append((file_id, code, exam, prob, collisions, number_slots, cpu_time))
                        new_file = False
                remove_file(file_name, linux=False)
                file_id += 1

    #print("Saving data", end="\n\n")
    #save_data(output_data, "output")


    print("<-----END----->")


if __name__ == '__main__':
    main()
