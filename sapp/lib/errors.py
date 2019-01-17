#!/usr/bin/env python3

TYPECHECK_ERROR = 102


class AIException(Exception):
    pass


class AIRecoverableException(AIException):
    pass


class AIProcessException(AIRecoverableException):
    def __init__(self, message, error_code):
        super().__init__(message)
        self.error_code = error_code


class ParseTypeException(Exception):
    pass
