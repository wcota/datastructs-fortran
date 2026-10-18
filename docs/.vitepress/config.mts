import { defineConfig } from 'vitepress'
import apiSidebar from '../api/_sidebar.json'

const base = process.env.BASE_PATH || '/datastructs-fortran/'

export default defineConfig({
  title: 'datastructs-fortran',
  description: 'Common data structures, high-performance sorting, and random samplers for Fortran',
  base: base,
  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      'fortran': 'fortran-free-form',
      'f90': 'fortran-free-form',
      'f95': 'fortran-free-form',
      'f03': 'fortran-free-form',
      'f08': 'fortran-free-form',
      'f77': 'fortran-fixed-form',
    },
  },
  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'API Reference', link: '/api/' },
    ],
    sidebar: {
      '/api/': [
        {
          text: 'API Reference',
          items: [
            { text: 'Overview', link: '/api/' },
          ],
        },
        ...apiSidebar,
      ],
    },
    socialLinks: [
      { icon: 'github', link: 'https://github.com/wcota/datastructs-fortran' },
    ],
    search: {
      provider: 'local',
    },
    footer: {
      message: 'Released under the LGPL-3.0 License.',
      copyright: 'Copyright © 2025-present Wesley Cota',
    },
  },
})
