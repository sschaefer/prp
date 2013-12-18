#!/usr/bin/python
# -*- coding: utf-8 -*-

import requests
import unittest

class TestPaperAPI(unittest.TestCase):

    def setUp(self):
        self.base_url = "http://localhost:8080"
        self.paper_url = self.base_url + "/paper/"

    def test_00_on_root_returns_html_hello_world(self):
        resp = requests.get(self.base_url)
        self.assertEqual(resp.content, "<html><body>Hello, new world.</body></html>")

    def test_01_on_paper_returns_id_in_html(self):
        for id in 1, 2, 3:
            resp = requests.get(self.paper_url + str(id))
            self.assertEqual(resp.status_code, 200)
            self.assertEqual(resp.content, "<html><body>" + str(id) + "</body></html>")

    def test_02_get_on_nonexisting_paper_returns_404(self):
        resp = requests.get(self.paper_url + '12345')
        self.assertEqual(resp.status_code, 404)

if __name__ == "__main__":
    suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestPaperAPI)
    unittest.TextTestRunner.run(suite)
