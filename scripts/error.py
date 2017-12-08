# Copyright 2004-present Facebook.  All rights reserved.


class Error:
    def __init__(self, external=False, **error):
        self.line = error['line']
        self.column = error['column']
        self.path = error['path']
        self.code = error['code']
        self.name = error['name']
        self.description = error['description']
        self.inference = error['inference']
        self.external = external

    def __repr__(self):
        return self.__key() + ' ' + self.description

    def __key(self):
        return self.path + ':' + str(self.line) + ':' + str(self.column)

    def __eq__(self, other):
        return self.__key() == other.__key()

    def __lt__(self, other):
        return self.__key() < other.__key()

    def __hash__(self):
        return hash(self.__key())

    def is_external(self):
        return self.external
