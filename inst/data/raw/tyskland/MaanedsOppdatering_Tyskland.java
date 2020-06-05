/*
bruker 3 filer:
	stasjoner_korr_sv - inneholder de stasjoner som fins i rimfrost
	svenske_stasjoner - inneholder de stasjonernr som har oppdatering
	..._temp - korresponderende temperatur tilhørende stas


*/
import javax.swing.* ;
import java.io.* ;
import java.util.* ;
class MaanedsOppdatering_Tyskland
{
	public static void main(String[] a) throws Exception
	{
		String korrespondtab[][] = new String[500][3] ;
		int linjenr = 0 ;
		FileReader mf = new FileReader("stasjoner_korr_ty.txt") ;
		BufferedReader leser1 = new BufferedReader(mf) ;

		String linje = leser1.readLine() ;
		while(linje!=null)
		{
			System.out.println(linje) ;
			StringTokenizer ord = new StringTokenizer(linje) ;
			int jj = 0 ;
			while(ord.hasMoreTokens())
			{
				korrespondtab[linjenr][jj] = ord.nextToken() ;
				jj++ ;
			}
			linjenr++ ;
			linje =leser1.readLine() ;
		}
		leser1.close() ;
		System.out.println("Korrespondansetabell etablert med : " + linjenr + " rader") ;
		
		FileReader statsfil = new FileReader("tyske_stasjoner.txt") ;
		BufferedReader statsleser = new BufferedReader(statsfil) ;

		String[] statstab = new String[200] ;
		String statsnr = statsleser.readLine() ;
		int ant=0 , antall=0;
		while(statsnr!=null)
		{
			statstab[ant] = statsnr ;
			ant++ ;
			statsnr = statsleser.readLine() ;
		}
		statsleser.close() ;
		System.out.println("Aktuell stasjonstabell etablert med : " + ant + " rader") ;		
		
		FileReader tempfil = new FileReader("nov_2012_temp.txt") ;
		BufferedReader templeser = new BufferedReader(tempfil) ;

		String[] temptab = new String[200] ;
		String temp = templeser.readLine() ;
		int anttemp=0 ;
		while(temp!=null)
		{
			temptab[anttemp] = temp ;
			anttemp++ ;
			temp = templeser.readLine() ;
		}
		templeser.close() ;
		System.out.println("Temperaturtabell etablert med : " + anttemp + " rader") ;
		System.out.println("Linjer i aktuell stasjonstabell   : " + ant) ;
		System.out.println("Linjer i aktuell temperaturtabell : " + anttemp) ;		
		
		int aar = Integer.parseInt(JOptionPane.showInputDialog("Angi året : ")) ;
		int mnd = Integer.parseInt(JOptionPane.showInputDialog("Angi måneden : ")) ;

		
		for(int i = 0;i<linjenr;i++)
		{
			if(korrespondtab[i][1].equals("1"))
			{	
				System.out.print("Rimfrost stasjon : " + korrespondtab[i][0] + " (" + korrespondtab[i][2]+")") ;
				String utf = korrespondtab[i][2] ;
				
				int indeks=-1 ;
				for(int j=0;j<anttemp ;j++)
				{
					//System.out.println(korrespondtab[i][0] + "-"+ statstab[j]) ;
					if(korrespondtab[i][0].equals(statstab[j]))
						indeks = j ;
				}
				

				try
				{
					FileWriter utfil = new FileWriter(utf,true) ;
					PrintWriter skr = new PrintWriter(utfil) ;

					//System.out.println("Indeks : " + indeks) ;
					
					if(indeks >=0)
					{
					
						System.out.println(" oppdatert med verdien : " + temptab[indeks]) ;
					
						if(mnd == 1)
						{
							skr.println() ;
							skr.print(aar) ;
							skr.print('\t'+temptab[indeks]) ;
						}
						else
						{
							skr.print('\t'+temptab[indeks]) ;
							
						}
						antall++ ;
					}
					System.out.println(utf + " oppdatert") ;
					skr.close() ;
					
				}
				catch(Exception e)
				{
					System.out.println("Finner ikke filen : "+ utf) ;
				}
				
			}
			
		}
		System.out.println("Totalt antall filer oppdatert : " + antall) ;

		
			


	}

}