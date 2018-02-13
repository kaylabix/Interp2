all:
	JULIA_LOAD_PATH=. julia ./ExtInt.jl
grade:
	JULIA_LOAD_PATH=. python ./extinter_solo_grader.py ./ExtInt.jl
	cat ./feedback.txt
