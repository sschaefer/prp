#!/usr/bin/python
# -*- coding: utf-8 -*-

import requests
import unittest

class TestPaperAPI(unittest.TestCase):

    def setUp(self):
        self.base_url = "http://localhost:8080"
        self.paper_url = self.base_url + "/paper/"
        self.json_headers = {"Content-Type": "application/json", "Accept": "application/json"}
        self.newpaper = {"title": "ABC"}
        self.newpaper2 = {"title": "DEF"}

    def test_00_get_on_root_returns_html_hello_world(self):
        resp = requests.get(self.base_url)
        self.assertEqual(resp.content, "<html><body>Hello, new world.</body></html>")

    def test_01_get_on_paper_returns_id_in_html(self):
        for id in 1, 2, 3:
            resp = requests.get(self.paper_url + str(id))
            self.assertEqual(resp.status_code, 200)
            self.assertEqual(resp.content, "<html><body>" + str(id) + "</body></html>")

    def test_02_get_on_nonexisting_paper_returns_404(self):
        resp = requests.get(self.paper_url + '12345')
        self.assertEqual(resp.status_code, 404)

    def test_03_get_on_paper_returns_id_in_json(self):
        for id in 1, 2, 3:
            resp = requests.get(self.paper_url + str(id), headers = self.json_headers)
            self.assertEqual(resp.status_code, 200)
            self.assertEqual(resp.content, '{"id":"' + str(id) + '","title":"' + str(id) + '"}')

    def test_04_put_new_paper(self):
        url = self.paper_url + "0"
        
        resp = requests.put(url, data=self.newpaper, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        # test durablility
        resp2 = requests.get(url, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 200)
        self.assertEqual(resp2.content, '{"id":"0","title":"ABC"}')

    def test_05_put_updates_paper(self):
        url = self.paper_url + '0'
        resp = requests.get(url)
        self.assertEqual(resp.status_code, 200)
        
        resp2 = requests.put(url, data=self.newpaper2, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 204)

    def test_06_post_new_paper_with_id(self):
        url = self.paper_url + "4"

        resp = requests.post(url, data=self.newpaper, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertNotEqual(resp.headers["location"], None)
        # test durablility
        new_url = self.base_url + resp.headers["location"]
        new_id = resp.headers["location"][7:] # remove "/paper/"
        resp2 = requests.get(new_url, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 200)
        self.assertEqual(resp2.content, '{"id":"' + new_id + '","title":"ABC"}')
        
    def test_07_delete_paper(self):
        # a previous test created /paper/0; now we delete it
        url = self.paper_url + "0"
        resp = requests.get(url, headers=self.json_headers)
        self.assertEqual(resp.status_code, 200)

        resp1 = requests.delete(url)
        self.assertEqual(resp1.status_code, 204)
        
        # test for durable:
        resp2 = requests.get(url, headers=self.json_headers)
        self.assertEqual(resp2.status_code, 404)

    def test_08_post_new_paper_without_id(self):
        url = self.paper_url
        resp = requests.post(url, data=self.newpaper, headers=self.json_headers)
        self.assertEqual(resp.status_code, 201)
        self.assertNotEqual(resp.content, '')

    def test_09_put_without_data_is_malformed(self):
        url = self.paper_url + '1'
        resp = requests.put(url, data={}, headers=self.json_headers)
        self.assertEqual(resp.status_code, 400)

    # this is an innovation on usual practice, corresponding neither to
    # recommendations of
    # https://en.wikipedia.org/wiki/RESTful nor
    # http://apigee.com/about/api-best-practices
    def test_10_post_specific_paper_creates_another(self):
        url = self.paper_url + '1'
        # When cowboy responds to POST with {{true, <<"new url">>}, ...}, it generates a 303 response
        # python retries the request at the new location but but fails to include the data
        # causing the POST to fail.  But if the POST had succeded, it would return 303 again, and loop
        # forever.  So don't allow cowyboy to respond for itself: force the 201.
        resp = requests.post(url, data=self.newpaper, headers=self.json_headers)

        self.assertEqual(resp.status_code, 201)
        # Bogus response from webmachine?
        # self.assertEqual(resp.content, '{"id":"1","title":"ABC"}')
        
        # New one was created
        self.assertNotEqual(resp.headers["location"], None)
        # The new one is not the one the post was applied to
        self.assertNotEqual(resp.headers["location"], self.paper_url + '1')
        new_id = resp.headers["location"][7:] # remove "/paper/"
        self.assertEqual(resp.content, '{"id":"' + new_id + '","title":"ABC"}')

if __name__ == "__main__":
    suite = unittest.TestLoader(verbosity=2).loadTestsFromTestCase(TestPaperAPI)
    unittest.TextTestRunner.run(suite)
