#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import re


def format_cpp_testcase(input: str) -> str:
    lines = input.splitlines()

    def line_process(s: str) -> str:
        if s.startswith("输出") or s.startswith("Output"):
            return "// " + s
        else:
            if s.startswith("Input"):
                s = s[6:]
            else:
                s = s[3:]
            s = re.sub(r"\[", "{", s)
            s = re.sub(r"\]", "}", s)
            s = re.sub(r", (\w) =", r"; \1 =", s)
            s = s + ";"
            return s

    output = "\n".join(
        [
            line_process(s)
            for s in lines
            if s.startswith("输") or s.startswith("Input") or s.startswith("Output")
        ]
    )
    return output


def format_rust_testcase(input: str) -> str:
    lines = input.splitlines()

    def line_process(s: str) -> str:
        if s.startswith("输出") or s.startswith("Output"):
            return "// " + s
        else:
            if s.startswith("Input"):
                s = s[7:]
            else:
                s = s[3:]
            s = re.sub(r"\[", "vec![", s)
            s = re.sub(r", (\w+) =", r"; let \1 =", s)
            s = re.sub(r"[^t] (\w+) =", r"; let \1 =", s)
            s = re.sub(r'(".*?")', r"\1.into()", s)
            s = "let " + s + ";"
            variables = re.findall(r"let \w+", s)
            for var in variables:
                snake = camel_to_snake(var[4:])
                s = s.replace(var[4:], snake)

            return s

    output = "\n".join(
        [
            line_process(s)
            for s in lines
            if s.startswith("输") or s.startswith("Input") or s.startswith("Output")
        ]
    )
    return output


def camel_to_snake(s: str):
    return "".join(["_" + i.lower() if i.isupper() else i for i in s]).lstrip("_")


def main():
    if len(sys.argv) != 3:
        return
    lang = sys.argv[1]
    testcase = sys.argv[2]
    if lang == "rust":
        print(format_rust_testcase(testcase))
    elif lang == "cpp":
        print(format_cpp_testcase(testcase))


if __name__ == "__main__":
    main()
